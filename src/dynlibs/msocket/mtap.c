/*
   Posix network tunnel interface.

   This program is intended to allow almost any programming language
   to do network I/O at the Ethernet/IP level on a Posix system.

   It simply relays data between its stdin/stdout, and a
   /dev/{tun,tap}N device, which is typically one end of a
   point-to-point tunnel carrying some sort of bridged traffic. It
   does this in a moderately efficient manner via a buffer which can
   be relatively enormous.  It doesn't restrict to tunnels though: if
   you turn on IP forwarding in the kernel, then the mutant TCP
   packets your node.js/v8 NFSv5 server emits can go terrorize the
   world ...

   All the scripting language needs to be able to do is exec a child
   process and talk to its stdin/stdout file descriptors via
   read/write. If it can send signals then that is all the better, but
   by no means is it necessary.

   Since /dev/{tun,tap} devices are usually only startable-uppable
   by root, this program tries to be careful enough to convince any
   sys-admin who cares to inspect it, that if he lets you run it
   set-uid root then it won't be _his_ fault when your institution
   gets hacked. Of course it _will_ be his fault ...

   Compile it with something like this on Linux:

      cc -o mtap -DLINUX -lrt mtap.c

   or on OpenBSD:

      cc -o mtap -DOPENBSD mtap.c

   and if you are using it set-uid $SUID

      sudo -v
      test -f ./mtap && sudo chown $USER ./mtap
      cc -o mtap -DLINUX -lrt mtap.c &&
      sudo chown $SUID ./mtap &&
      sudo chmod +s ./mtap

   On Linux you need something like this in /etc/net-tun-up

      #! /bin/sh
      ifconfig $1 inet 192.168.0.1 netmask 255.255.255.0

   On OpenBSD you can use something like:

      #! /bin/sh
      ifconfig `basename $1` inet 10.0.0.1 10.0.0.2

   In either case, if it's set-uid, then the script needs to be owned
   by root and on a filesystem not mounted nosuid. Whether or not it's
   set-uid, it must have the relevant executable permission bits set.

   Then if mtap is set-uid root, you can run it with something like (OpenBSD)

      /usr/bin/mtap -vvvv -U /etc/net-tun-up /dev/tun1

   and on Linux:

      /usr/bin/mtap -vvvv -U /etc/net-tun-up 

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include <sys/ioctl.h>
#include <sys/uio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <fcntl.h>

#include <sys/wait.h>
#include <libgen.h>

#include <sys/time.h>
#include <signal.h>
#include <sys/socket.h>
#include <net/route.h>
#include <net/if.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#include <sys/mman.h>

#ifdef LINUX
#include <linux/if_tun.h>
#define PATH_NET_TUN "/dev/net/tun"
#endif

#ifdef OPENBSD
#include <net/if_tun.h>
#endif

#ifdef OPENBSD
#define si_int si_value.sival_int
#define SIGDEF1 SIGUSR1
#define SIGDEF2 SIGUSR2
#else
#define SIGDEF1 SIGRTMIN
#define SIGDEF2 (SIGRTMIN+1)
#endif

static int verbose = 1;

static void pstrcpy(char *dst, size_t len, const char *src)
{
#ifdef OPENBSD
   strlcpy (dst,src,len);
#else
   strncpy(dst, src, len);

   if (verbose > 3) {
     fprintf(stderr,"copied src=%s,dst=%s,max=%d]\n",dst,src,len);
   }

   if (len > 0)
      dst[len - 1] = '\0';
#endif
}

#define TEMPORARILY 0
#define PERMANENTLY 1

static void drop_privs(int permanently, uid_t uid, uid_t euid)
{
   if (verbose >= 1) {
      fprintf(stderr, "uid=%d,euid=%d.\n",uid,euid);
   }
   if (euid != uid) {
     if (permanently == 0) {
       if (verbose >= 1) {
          fprintf(stderr, "dropping privs temporarily to uid=%d, saving euid=%d\n",uid,euid);
       }
       if (setresuid(uid,uid,euid) < 0) {
          perror("Failed to save euid");
          exit(EXIT_FAILURE);
       }
     } else { /* permanently */
         if (verbose >= 1) {
            fprintf(stderr, "dropping privs permanently to uid=%d\n",uid);
         } 
         if (setuid(uid) < 0) {
             perror("Failed to drop privs");
             exit(EXIT_FAILURE);
         }
      }
   }
}

static void raise_privs(uid_t uid, uid_t euid)
{
   if (euid != uid) {
     if (verbose >= 1) {
       fprintf(stderr, "reasserting privs to euid=%d, saving euid=%d\n",euid,euid);
     } 
     if (setresuid(uid,euid,euid) < 0) {
        perror("Failed to restore euid");
        exit(EXIT_FAILURE);
     }
   }
}

static int check_script_perms(const char *path) {
    struct stat sb;
    char *pathcpy, *dname;
    uid_t uid;
    uid_t euid;
    gid_t egid;

    uid = getuid();
    euid = geteuid();
    egid = getegid();

    if (verbose > 3) {
       fprintf(stderr, "checking permissions on %s\n", path);
    } 
    if (stat(path, &sb) != 0) {
       perror("stat()");
       if (verbose > 3) {
           fprintf(stderr, "can't stat %s.\n", path);
        } 
       return -1;
    }
    if (S_ISREG(sb.st_mode) == 0) {
        fprintf(stderr, "the path %s is not a regular file.\n", path);
        return -1;
    }
    if (sb.st_uid != euid && sb.st_uid != 0) {
      fprintf(stderr, "the script %s is not owned by root or by the efective uid (%d).\n", path,euid);
        return -1;
    }
    if (S_IWOTH & sb.st_mode != 0) {
        fprintf(stderr, "the scipt %s is world-writable.\n", path);
        return -1;
    }
    if (sb.st_gid != 0 && sb.st_gid != egid && (S_IWGRP & sb.st_mode) != 0) {
        fprintf(stderr, "the scipt %s is group-writable and "
                       "that group is neither wheel nor the effective gid (%d).\n", path, egid);
        return -1;
    }
#ifndef GET_A_LIFE
    if ((S_ISUID & sb.st_mode) != 0 ||
        ((S_ISGID & sb.st_mode) != 0 && (S_IXGRP | sb.st_mode) != 0)) {
        fprintf(stderr, "the script %s is setuid/setgid.\n", path);
        return -1;
    }
#endif
    if ((pathcpy = strdup(path)) == NULL) {
       fprintf(stderr, "out of memory.\n");
       return -1;
    }
    dname = dirname(pathcpy);
    if (stat(dname, &sb) != 0) {
       perror("stat()");
       if (verbose > 3) {
           fprintf(stderr, "can't stat %s.\n", dname);
       }
       return -1;
    }
    if (S_ISDIR(sb.st_mode) == 0) {
        fprintf(stderr, "the path %s is not a directory.\n", dname);
        return -1;
    }
    if (sb.st_uid != 0 && sb.st_uid != euid) {
      fprintf(stderr, "the directory %s is not owned by"
                      " either root or the effective uid (%d).\n", dname, euid);
        return -1;
    }
    if ((S_IWOTH & sb.st_mode) != 0) {
        fprintf(stderr, "the directory %s is world-writable.\n", dname);
        return -1;
    }
    if (sb.st_gid != 0 && sb.st_gid != egid && (S_IWGRP & sb.st_mode) != 0) {
        fprintf(stderr, "the directory %s is group-writable and"
                " the group is not wheel or the effective gid (%d).\n", dname,egid);
        return -1;
    }    
    if (verbose > 3) {
       fprintf(stderr, "permissions on %s seem OK.\n", path);
    }
    return 0;
}

#ifdef LINUX
static int tap_init(char *ifname, size_t ifname_size, int mode)
{
    struct ifreq ifr;
    int fd, ret;
    int open_mode = O_RDWR;

    if (verbose > 3) {
       fprintf(stderr, "opening %s ...\n", PATH_NET_TUN);
    } 
    fd = open(PATH_NET_TUN, open_mode);
    if (fd < 0) {
        fprintf(stderr, "could not open %s: %s\n", PATH_NET_TUN, strerror(errno));
        return -1;
    }
    if (verbose > 3) {
       fprintf(stderr, "open of %s succeeded\n", PATH_NET_TUN);
    } 
    memset(&ifr, 0, sizeof(ifr));
    ifr.ifr_flags = mode;

    if (ifname[0] != '\0')
        pstrcpy(ifr.ifr_name, IFNAMSIZ, ifname);
    else
        pstrcpy(ifr.ifr_name, IFNAMSIZ, (mode & IFF_TAP ? "tap%d" : "tun%d"));

    ret = ioctl(fd, TUNSETIFF, (void *) &ifr);

    if (ret != 0) {
        fprintf(stderr,"could not configure %s (%s): %s\n",
                PATH_NET_TUN, ifr.ifr_name,strerror(errno));
        close(fd);
        return -1;
    }
    pstrcpy(ifname, IFNAMSIZ, ifr.ifr_name);
    if (verbose > 3) {
      fprintf(stderr, "configuration of %s (index=%d) succeeded\n",
                ifname, ifr.ifr_ifindex);
    }
    return fd;
}
#endif

#ifdef OPENBSD
int tap_init (const char *ifname)
{
    int fd, ret;
    int open_mode = O_RDWR;

    if ((fd = open (ifname,open_mode)) == -1) {
       fprintf(stderr,"can't open device %s (%s)\n",ifname,strerror(errno));
       return -1;
    }
    if (verbose > 3) {
       fprintf(stderr, "configuration of %s succeeded\n", ifname);
    }
    return fd;
}
#endif

static int launch_script(char *script, char *ifname)
{
    int pid, status;
    char *args[3] = {script,ifname,NULL};
    char *env[1] = {NULL};

    if (verbose > 0) {
       fprintf(stderr, "running %s ...\n", script);
    }
    if ((pid = fork()) == 0) {
      execve(script, args, env); /* execve is supposed to set the saved 
                                    set-uid to the effective uid, preventing
                                    the script from re-elevating to any saved
                                    set-uid privs. This saved/effective uid 
                                    interface is _way_ too complex to be practical. */
      _exit(1);
    } else if (pid > 0) {
        while (waitpid(pid, &status, 0) != pid); /* whiling away the time ... */
        if (WIFEXITED(status) && WEXITSTATUS(status) == 0) { /* until wife xited. */
           if (verbose > 0) {
              fprintf(stderr, "script %s succeeded\n", script);
            } 
            return 0;
        }
    }
    fprintf(stderr, "script %s failed. Status %d\n", script,status);
    return -1;
}

int tap_shutdown(char *script, char *ifname, int fd)
{
   if (verbose > 3) {
      fprintf(stderr, "shutting down i/f %s\n", ifname);
   } 
   if (close(fd) == -1) {
      fprintf(stderr, "close failed: errno = %d (%s)\n", errno, strerror(errno));
      return -1;
   }
   if (script != NULL && *script != '\0' && strcmp(script, "none") != 0) {
     if (launch_script(script, ifname) != 0)
        fprintf(stderr, "shutdown script %s failed\n", script);
   }
   if (verbose > 3) {
     fprintf(stderr, "shutdown of %s succeeded\n", ifname);
   } 
   return 0;
}

static int done = 0;

static void int_handler(int sig, siginfo_t *si, void *unused)
{
  if (sig == SIGINT)
     done = 1;
  return;
}

#define FDBUFF 1
#define SHMBUFF 2

/* OpenBSD's posix signal interface doesn't include sigqueue,
   so we can't send data with signals */
#ifdef OPENBSD
#define SIGNALIO 0
#else
#define SIGNALIO 4
#endif

unsigned int txbuf_written = 0U;
unsigned int rxbuf_written = 0U;

unsigned int txbuf_read = 0U;
unsigned int rxbuf_read = 0U;

static int txsig=0;
static int rxsig=0;

static void txsig_handler(int sig, siginfo_t *si, void *unused)
{
  if (sig == txsig)
     txbuf_written = si->si_int;
  return;
}

static void rxsig_handler(int sig, siginfo_t *si, void *unused)
{
  if (sig == rxsig)
     rxbuf_read = si->si_int;
  return;
}

#ifdef OPENBSD
#define sigqueue(p,s,v) (kill(p,s))
#endif

static void queue_txsig(pid,i) {
  union sigval sv;
  sv.sival_int = i;
  if (sigqueue(pid, txsig, sv) == -1) {
        perror("sigqueue txsig");
        exit(EXIT_FAILURE);
  }
  if (verbose > 4) {
    fprintf(stderr, "queue_txsig: queued %d[%d] with sival_int=%d\n",txsig,pid,i);
  } 
}

static void queue_rxsig(pid,i) {
  union sigval sv;
  sv.sival_int = i;
  if (sigqueue(pid, rxsig, sv) == -1) {
        perror("sigqueue rxsig");
        exit(EXIT_FAILURE);
  }
  if (verbose > 4) {
    fprintf(stderr, "queue_rxsig: queued %d[%d] with sival_int=%d\n",rxsig,pid,i);
  } 
}

#define max(x,y) ((x) > (y) ? (x) : (y))
#define min(x,y) ((x) < (y) ? (x) : (y))

static int do_io
(long to, pid_t pid, int mode,
 int fd0,
 int fd1, char *txbuf, unsigned int txbuf_size,
 int fd2, char *rxbuf, unsigned int rxbuf_size)
{
   unsigned int txbuf_mask;
   unsigned int rxbuf_mask;

#define txbuf_roff (txbuf_read & txbuf_mask)
#define rxbuf_roff (rxbuf_read & rxbuf_mask)

#define txbuf_woff (txbuf_written & txbuf_mask)
#define rxbuf_woff (rxbuf_written & rxbuf_mask)

#define txbuf_avail (txbuf_read - txbuf_written)
#define rxbuf_avail (rxbuf_read - rxbuf_written)

#define txbuf_free (txbuf_size - txbuf_avail)
#define rxbuf_free (rxbuf_size - rxbuf_avail)

   struct timeval tv;

   struct sigaction sa;

   sigset_t sigs;
   sigset_t oldsigs;

   struct iovec iovs[2];
   int niovs;

   sigemptyset(&sigs);

   txbuf_written = txbuf_read = 0U;
   rxbuf_written = rxbuf_read = 0U;

   txbuf_mask = txbuf_size - 1U;
   rxbuf_mask = rxbuf_size - 1U;

   if (verbose > 3) {
     fprintf(stderr, "txbuf: size=%u, mask=%x\n",txbuf_size,txbuf_mask);
     fprintf(stderr, "rxbuf: size=%u, mask=%x\n",rxbuf_size,rxbuf_mask);
   } 

   if (verbose > 3) {
      fprintf(stderr, "starting io loop\n");
   } 
   sa.sa_flags = SA_SIGINFO;
   sigemptyset(&sa.sa_mask);
   sa.sa_sigaction = int_handler;
   if (sigaction(SIGINT, &sa, NULL) == -1) {
        perror("sigaction: SIGINT");
        exit(EXIT_FAILURE);
   }
   if (sigaddset(&sigs,SIGINT) == -1) {
        perror("sigaddset: SIGINT");
        exit(EXIT_FAILURE);
   }
   if ((mode & SIGNALIO) != 0) {
      sa.sa_flags = SA_SIGINFO;
      sigemptyset(&sa.sa_mask);
      sa.sa_sigaction = txsig_handler;
      if (sigaction(txsig, &sa, NULL) == -1) {
           perror("sigaction: txsig");
           exit(EXIT_FAILURE);
      }
      if (sigaddset(&sigs,txsig) == -1) {
           perror("sigaddset: txsig");
           exit(EXIT_FAILURE);
      }
      sa.sa_flags = SA_SIGINFO;
      sigemptyset(&sa.sa_mask);
      sa.sa_sigaction = rxsig_handler;
      if (sigaction(rxsig, &sa, NULL) == -1) {
           perror("sigaction: rxsig");
           exit(EXIT_FAILURE);
      }
      if (sigaddset(&sigs,rxsig) == -1) {
           perror("sigaddset: rxsig");
           exit(EXIT_FAILURE);
      }
   }
   if (sigprocmask(SIG_UNBLOCK, &sigs, &oldsigs) == -1) {
          perror("sigprocmask(SIG_UNBLOCK,&sigs)");
          exit(EXIT_FAILURE);
   }
   if (verbose > 3) {
      fprintf(stderr, "signals initialized.\n");
   } 
   for (done=0;done==0;) {
       int r, nfds = 0;
       fd_set rd, wr, er;
       int errsv;

       FD_ZERO(&rd);
       FD_ZERO(&wr);
       FD_ZERO(&er);

       if (verbose > 4) {
          fprintf(stderr, "start of loop:\n");
          fprintf(stderr, "  txbuf: avail=%u, free=%u, "
                          "roff=%u, woff=%u, read=%u, written=%u\n",
                                    txbuf_avail, txbuf_free,
                           txbuf_roff, txbuf_woff, txbuf_read, txbuf_written);
          fprintf(stderr, "  rxbuf: avail=%u, free=%u, "
                          "roff=%u, woff=%u, read=%u, written=%u\n",
                                    rxbuf_avail, rxbuf_free,
                           rxbuf_roff, rxbuf_woff, rxbuf_read, rxbuf_written);
       }
       if ((mode & SIGNALIO) == 0) {
          if (verbose > 4) {
             fprintf(stderr, "mode is FDIO\n");
          } 
          if (txbuf_free > 0) {
             FD_SET(fd0, &rd);
             nfds = max(nfds, fd0);
             if (verbose > 4) {
                fprintf(stderr, "fd0 read poll: rxbuf %u\n",txbuf_free);
             } 
          }
          if (rxbuf_avail > 0) {
              FD_SET(fd1, &wr);
              nfds = max(nfds, fd1);
              if (verbose > 4) {
                 fprintf(stderr, "fd1 write poll: txbuf %u\n", rxbuf_avail);
              }
          }
       }
       if (rxbuf_free > 0) {
           FD_SET(fd2, &rd);
           nfds = max(nfds, fd2);
           if (verbose > 4) {
              fprintf(stderr, "fd2 read poll: txbuf %u\n",rxbuf_free);
           } 
       }
       if (txbuf_avail > 0) {
           FD_SET(fd2, &wr);
           nfds = max(nfds, fd2);
           if (verbose > 4) {
              fprintf(stderr, "fd2 write poll: rxbuf %u\n", txbuf_avail);
           } 
       }
       if (done == 1)
           continue;
       if (verbose > 3) {
         fprintf(stderr, "calling select with t/o of %ld seconds ...",to);
       } 

       tv.tv_sec = to;
       tv.tv_usec = 0L;

       r = select(nfds + 1, &rd, &wr, &er, &tv);

       errsv = errno;

       if (verbose > 4) {
          fprintf(stderr, "\nselect returned %d \"%s\"\n",errsv,errsv == 0 ? "Success" : strerror(errsv));
       } 
       if (r == -1 && errno == EINTR)
          continue;
       if (r == -1) {
           perror("select()");
           exit(EXIT_FAILURE);
       }
       if (verbose > 4) {
          fprintf(stderr, "checking fds ...\n");
       } 
       if (FD_ISSET(fd0, &rd)) {
           unsigned int count;
           unsigned int txfree;
           txfree = txbuf_free;

           if (mode & FDBUFF)
              count = min (txbuf_size - txbuf_roff, txfree);
           else
              count = txfree;

           iovs[0].iov_base = txbuf + txbuf_roff;
           iovs[0].iov_len = count;
           niovs = 1;
           if (count < txfree) {
              iovs[1].iov_base = txbuf;
              iovs[1].iov_len = txfree - count;
              niovs = 2;
           }
           if (verbose > 4) {
             fprintf(stderr, "fd0: reading: %u in %d iovs into txbuf\n", txfree,niovs);
           } 
           r = readv(fd1, iovs, niovs);
           if (r == -1) {
              perror("read fd0");
              exit(EXIT_FAILURE);
           } else {
               txbuf_read += r;
           }
           if (verbose > 4) {
             fprintf(stderr, "   read %d: avail = %u\n",r,txbuf_avail);
           } 
       }
       if (FD_ISSET(fd1, &wr)) {
           unsigned int count;
           unsigned int avail;

           avail = rxbuf_avail;

           if (mode & FDBUFF)
              count = min (rxbuf_size - rxbuf_woff, avail);
           else
              count = avail;

           iovs[0].iov_base = rxbuf + rxbuf_woff;
           iovs[0].iov_len = count;
           niovs = 1;
           if (count < avail) {
              iovs[1].iov_base = rxbuf;
              iovs[1].iov_len = avail - count;
              niovs = 2;
           }
           if (verbose > 4) {
             fprintf(stderr, "fd1: writing: %u in %d iovs into rxbuf\n", avail,niovs);
           } 
           r = writev(fd1, iovs, niovs);
           if (r == -1) {
              perror("write fd1");
              exit(EXIT_FAILURE);
           } else {
               rxbuf_written += r;
           }
           if (verbose > 4) {
             fprintf(stderr, "   wrote %d: written = %u\n",r,rxbuf_written);
           } 
       }
       if (FD_ISSET(fd2, &rd)) {
           unsigned int count;
           unsigned int rxfree;

           rxfree = rxbuf_free;

           if (mode & FDBUFF)
              count = min (rxbuf_size - rxbuf_roff, rxfree);
           else
              count = rxfree;

           iovs[0].iov_base = rxbuf + rxbuf_roff;
           iovs[0].iov_len = count;
           niovs = 1;
           if (count < rxfree) {
              iovs[1].iov_base = rxbuf;
              iovs[1].iov_len = rxfree - count;
              niovs = 2;
           }
           if (verbose > 4) {
             fprintf(stderr, "fd2: reading: %u in %d iovs into rxbuf\n", rxfree,niovs);
           } 
           r = readv(fd2, iovs,niovs);
           if (r == -1) {
              perror("read fd2");
              exit(EXIT_FAILURE);
           } else {
               rxbuf_read += r;
               if (mode & SIGNALIO)
                  queue_rxsig(pid,rxbuf_read);
           }
           if (verbose > 4) {
             fprintf(stderr, "   read %d: avail = %u\n",r,rxbuf_avail);
           } 
       }
       if (FD_ISSET(fd2, &wr)) {
           unsigned count;
           unsigned int avail;
           avail = txbuf_avail;

           if (mode & FDBUFF)
              count = min (txbuf_size - txbuf_woff, avail);
           else
              count = avail;

           iovs[0].iov_base = txbuf + txbuf_woff;
           iovs[0].iov_len = count;
           niovs = 1;
           if (count < avail) {
              iovs[1].iov_base = txbuf;
              iovs[1].iov_len = avail - count;
              niovs = 2;
           }
           if (verbose > 4) {
             fprintf(stderr, "fd2: writing: %u in %d iovs into txbuf\n", avail,niovs);
           } 
           r = writev(fd2, iovs,niovs);

           if (r == -1) {
              perror("write fd2");
              exit(EXIT_FAILURE);
           } else {
               txbuf_written += r;
               if (mode & SIGNALIO)
                  queue_txsig(pid,txbuf_written);
           }
           if (verbose > 4) {
             fprintf(stderr, "   wrote %d: written = %u\n",r,txbuf_written);
           } 
       }
       if (verbose > 4) {
          fprintf(stderr, "read %u, wrote %u.\n",
                          txbuf_read+rxbuf_read,txbuf_written+rxbuf_written);
          fprintf(stderr, "looping.\n");
       }
   }
   if (verbose > 3) {
     fprintf(stderr, "leaving io loop: read %u, wrote %u.\n",
                     txbuf_read+rxbuf_read,txbuf_written+rxbuf_written);
   } 
   if (sigprocmask(SIG_SETMASK, &oldsigs, NULL) == -1) {
          perror("sigprocmask(SIG_SETMASK, &oldsigs)");
          exit(EXIT_FAILURE);
   }
   if (verbose > 3) {
     fprintf(stderr, "restored signal mask\n");
   }   
   return 0;
}

static void *allocbuf(size_t count) {
   void *buf;

   if (verbose > 4) {
     fprintf(stderr, "allocating %d bytes for buffer ... ",count);
   }
   if ((buf = malloc (count)) == NULL) {
     fprintf(stderr, "malloc failed\n");
     exit (EXIT_FAILURE);
   }
   if (verbose > 4) {
     fprintf(stderr, "Done.\n");
   } 
   return buf;
}

static int create_shared_mem(const char *shmname,
                             size_t count,
                             int shmflags,
                             int mode) {
   int fd;

   if (verbose > 4) {
     fprintf(stderr, "creating shared memory handle %s with"
                     " %d bytes for buffer\n",shmname,count);
   }
   if ((fd = shm_open (shmname,shmflags|O_CREAT,mode)) == -1) {
     perror("shm_open");
     fprintf(stderr, "can't create shared memory object %s\n",shmname);
     exit (EXIT_FAILURE);
   }
   if (verbose > 4) {
     fprintf(stderr, "truncating to %d bytes\n",count);
   }
   if (ftruncate (fd,count) == -1) {
     perror("ftruncate");
     exit (EXIT_FAILURE);
   }
   if (verbose > 4) {
     fprintf(stderr, "Created shared memory object %s\n",shmname);
   } 
   return fd;
}

static void *mapbuf(void *addr,
                    const char * shmname,
                    size_t count,
                    int shmflags,int mode,
                    int mflags,int prot) {
   void *buf=NULL, *buf1, *buf2;
   int fd=-1;

   if (verbose > 4) {
     fprintf(stderr, "mapping reserve address space\n");
   }
   if (addr == NULL) {
      if ((buf = mmap (NULL,count*2,PROT_NONE,
                       MAP_ANONYMOUS|MAP_PRIVATE|MAP_NORESERVE,-1,0)) == MAP_FAILED) {
         perror("mmap");
         fprintf(stderr, "can't map %d bytes of reserve address space for %s\n",count*2,shmname);
         goto err_exit0;
      }
      if (verbose > 4) {
        fprintf(stderr, "reserved %d bytes starting at %p\n",count*2,buf);
      }
   } else {
      buf = addr;
   }
   if (verbose > 4) {
     fprintf(stderr, "opening shared memory object %s\n",shmname);
   }
   if ((fd = shm_open (shmname,shmflags,mode)) == -1) {
     perror("shm_open");
     fprintf(stderr, "can't open shared memory object %s\n",shmname);
     goto err_exit1;
   }
   if (verbose > 4) {
     fprintf(stderr, "mapping %d bytes of shared memory fd %d at %p\n",count,fd,buf);
   }
   if ((buf1 = mmap ((char *)buf,count,prot,mflags|MAP_FIXED,fd,0)) == MAP_FAILED) {
     perror("mmap");
     fprintf(stderr, "can't map shared memory object %s\n",shmname);
     goto err_exit2;
   }
   if (verbose > 4) {
     fprintf(stderr, "mapped %d bytes at %p\n",count,buf1);
   }
   if (close (fd) == -1) {
     perror("close");
     fprintf(stderr, "can't close fd %d\n",fd);
     fd = -1;
     goto err_exit3;
   }
   if (verbose > 4) {
     fprintf(stderr, "shared memory fd %d closed.\n",fd);
   } 
   fd = -1;
   if (addr == NULL) {
      if (verbose > 4) {
        fprintf(stderr, "now mapping the same block [%p] again at %p.\n",
                         (char *) buf1,(char *) buf1 + count);
      } 
      if ((buf2 = mapbuf((char *) buf1 + count,shmname,count,shmflags,
                              mode,mflags|MAP_FIXED,prot)) == MAP_FAILED) {
         fprintf(stderr, "can't map shared memory object %s\n",shmname);
         goto err_exit3;
      }
      if (verbose > 4) {
        fprintf(stderr, "mapped %d bytes at %p and again at %p\n",count,buf1,buf2);
      }
   }
   return buf1;

 err_exit3: 
   (void) munmap (buf1,count);
 err_exit2: 
   if (fd != -1)
      (void) close (fd);
   (void) shm_unlink(shmname);
 err_exit1: 
   if (buf != NULL && addr == NULL)
      (void) munmap (buf,count*2);
 err_exit0: 
   return MAP_FAILED;
}

#define DEFIFUPSCRIPT "none"
#define DEFIFDOWNSCRIPT "none"
#define MAXVERB   5
#define MAXNAME 256
#define MAXBUF (8 * (sizeof(unsigned long)) - 1)
#define MAXTO (60L * 60L)
#define TAPDEV "tap%d"
#define TUNDEV "tun%d"
#define MAXSIG _NSIG
#define PAGESIZE sysconf(_SC_PAGE_SIZE)

#ifdef OPENBSD
#define IFNAMELENGTH MAXNAME
#else
#define IFNAMELENGTH IFNAMSIZ
#endif

static int minbits(int n) {
   int i;
   for (i = 0x0; i < n; i++)
     if ((1 << i) >= n)
        return i;
}

int main (int argc, char *argv[]) {
   int opt;
   int to;
   unsigned int txbuf_size, rxbuf_size;

   int txsig, rxsig;
   char *shmname, *bufshmname;
   int pagebits;

   char *up, *down;
   char *device;

   char *rstderr = NULL;
   int fderr = -1;

   uid_t uid, euid;

   char *txbuf, *rxbuf;

   int elevprivs_up,elevprivs_down;

   char upscript[MAXNAME];
   char downscript[MAXNAME];
   char ifname[IFNAMELENGTH+1];

   char txshmname[MAXNAME];
   char rxshmname[MAXNAME];

   int ifmode, iffmt;
   int mode;

   int fd;
   int ret;
   int serr;
   pid_t ppid;

#ifdef LINUX
   ifmode = IFF_TUN;
   iffmt = IFF_NO_PI;
#endif
   device = TUNDEV;

   shmname = NULL;
   bufshmname = NULL;
   pagebits = minbits(PAGESIZE);

   to = 30L;
   verbose = 1;
   txbuf_size = 1U << pagebits;
   rxbuf_size = 1U << pagebits;

   up = DEFIFUPSCRIPT;
   down = DEFIFDOWNSCRIPT;

   elevprivs_up = 0;
   elevprivs_down = 0;

   ppid = getppid();

   uid = getuid();
   euid = geteuid();

   drop_privs(TEMPORARILY,uid,euid);

#ifdef LINUX
#define OPTIONS "qvt:r:U:u:D:d:w:phe:s:T:R:S:"
#endif
#ifdef OPENBSD
#define OPTIONS "qvt:r:U:u:D:d:w:e:s:T:R:S:"
#endif
   while ((opt = getopt(argc, argv, OPTIONS)) != -1) {
       switch (opt) {
       case 'e':
           if (strlen(optarg) > (MAXNAME-20)) {
             fprintf(stderr, "-e argument too long (max %d chars)\n", MAXNAME-20);
             exit(EXIT_FAILURE);
           }
           rstderr = optarg;
           break;
       case 'S':
         if (shmname != NULL) {
             fprintf(stderr, "can't specify -S with -s\n");
             exit(EXIT_FAILURE);
         } else {
           if (strlen(optarg) > (MAXNAME-2)) {
             fprintf(stderr, "shared memory name argument (-%c) too long (limit is %d chars)\n",
                              opt,MAXNAME-2);
             exit(EXIT_FAILURE);
           }
           bufshmname = optarg;
           break;
         }
       case 's':
         if (bufshmname != NULL) {
             fprintf(stderr, "can't specify -s with -S\n");
             exit(EXIT_FAILURE);
         } else {
           if (strlen(optarg) > (MAXNAME-2)) {
             fprintf(stderr, "shared memory name argument (-%c) too long (limit is %d chars)\n",
                              opt,MAXNAME-2);
             exit(EXIT_FAILURE);
           }
           shmname = optarg;
           break;
         }
#ifdef LINUX
       case 'p':
           ifmode = IFF_TAP;
           device = TAPDEV;
           break;
       case 'h':
           iffmt = 0;
           break;
#endif
       case 'v':
           ++verbose;
           if (verbose > MAXVERB) {
             fprintf(stderr, "verbosity must be between 0 and %d\n", MAXVERB);
             exit(EXIT_FAILURE);
           }
           break;
       case 'q':
           verbose=0;
           break;
       case 'w':
           to = (long) atoi(optarg);
           if (to < 0 || to > MAXTO) {
             fprintf(stderr, "timeout must be between 0 and %ld seconds\n", MAXTO);
             exit(EXIT_FAILURE);
           }
           break;
       case 'T':
           txsig = atoi(optarg);
           if (txsig < 0 || txsig > MAXSIG) {
             fprintf(stderr, "tx signal number (-T) must be between 1 and %d\n", MAXSIG);
             exit(EXIT_FAILURE);
           }
           break;
       case 'R':
           rxsig = atoi(optarg);
           if (rxsig < 0 || txsig > MAXSIG) {
             fprintf(stderr, "rx signal number (-R) must be between 1 and %d\n", MAXSIG);
             exit(EXIT_FAILURE);
           }
           break;
       case 't':
           txbuf_size = (unsigned int) atoi(optarg);
           if (txbuf_size < 1 || txbuf_size > MAXBUF) {
             fprintf(stderr, "log2 txbuf size must be from 1 to %d bits\n", MAXBUF);
             exit(EXIT_FAILURE);
           }
           txbuf_size = 1U << txbuf_size;
           break;
       case 'r':
           rxbuf_size = (unsigned int) atoi(optarg);
           if (rxbuf_size < 1 || rxbuf_size > MAXBUF) {
             fprintf(stderr, "log2 rxbuf size must be from 1 to %d bits\n", MAXBUF);
             exit(EXIT_FAILURE);
           }
           rxbuf_size = 1U << rxbuf_size;
           break;
       case 'U':
         if (euid != uid)
            elevprivs_up = 1;
         else if (uid != 0) {
             fprintf(stderr, "-%c specified, but not running with elevated privs.\n", opt);
             exit(EXIT_FAILURE);
         }
       case 'u':
           if (strlen(optarg) > (MAXNAME-20)) {
             fprintf(stderr, "-%c argument too long (max %d chars)\n", opt, MAXNAME-20);
             exit(EXIT_FAILURE);
           }
           up = optarg;
           break;
       case 'D':
         if (euid != uid)
            elevprivs_down = 1;
         else if (uid != 0) {
             fprintf(stderr, "-%c specified, but not running with elevated privs.\n", opt);
             exit(EXIT_FAILURE);
         }
       case 'd':
           if (strlen(optarg) > (MAXNAME-20)) {
             fprintf(stderr, "-%c argument too long (max %d chars)\n", opt, MAXNAME-20);
             exit(EXIT_FAILURE);
           }
           down = optarg;
           break;
       default: /* '?' */
#ifdef LINUX
#define SIGOPTHELP  "[-s shmname [-T txsigno] [-R rxsigno]]"
#define OPTHELP "[-h] [-p] [device]"
#endif
#ifdef OPENBSD
#define SIGOPTHELP  "[-s shmname]"
#define OPTHELP "device"
#endif
           fprintf(stderr, "Usage: %s [-v[v[v[v]]]|-q] [-e stderrfile]\n"
                           "           [-S shmname] [-t log2_tx_bufsize] [-r log2_rx_bufsize]"
                                     " [-w seconds]\n"
                           "           %s\n"
                           "           [-[u|U] if-up-script|none] [-[d|D] if-down-script|none]\n"
                           "           %s\n"
                           "Now I suppose you want to know what they all do?\n",
                 argv[0],SIGOPTHELP,OPTHELP);
           exit(EXIT_FAILURE);
       }
   }
#ifdef LINUX
   if (optind == argc - 1) {
     if (strlen (argv[optind]) > (IFNAMELENGTH-4)) {
        fprintf(stderr, "device name [%s] too long (max %d chars)\n",argv[optind], IFNAMELENGTH-4);
        exit(EXIT_FAILURE);
     }
     device = argv[optind];
   } else if (optind < (argc - 1)) {
        fprintf(stderr, "too many arguments\n");
        exit(EXIT_FAILURE);
   }
#endif
#ifdef OPENBSD
   if (optind == argc - 1) {
     if (strlen (argv[optind]) > (IFNAMELENGTH-1)) {
        fprintf(stderr, "device name [%s] too long (max %d chars)\n",argv[optind], IFNAMELENGTH-1);
        exit(EXIT_FAILURE);
     }
     device = argv[optind];
     if (verbose > 3) {
        fprintf(stderr,"interface is %s\n",device);
     }
   } else if (optind == argc) {
        fprintf(stderr, "no device name specified\n");
        exit(EXIT_FAILURE);
   } else if (optind < (argc - 1)) {
        fprintf(stderr, "too many arguments\n");
        exit(EXIT_FAILURE);
   }
#endif
   if (shmname != NULL) {
     if (txsig == 0)
        txsig = SIGDEF1;
     if (rxsig == 0)
        rxsig = SIGDEF2;
     if (pagebits > txbuf_size) {
        fprintf(stderr,"the shared memory tx buffer size (-t) must be"
                " at least one page (%ld [=2^%d] bytes)\n",PAGESIZE,pagebits);
        exit(EXIT_FAILURE);
     }
     if (pagebits > rxbuf_size) {
        fprintf(stderr,"the shared memory rx buffer size (-r) must be"
                " at least one page (%ld [=2^%d] bytes)\n",PAGESIZE,pagebits);
        exit(EXIT_FAILURE);
     }
     if (verbose > 4) {
       fprintf(stderr, "mapping %d page(s) for shared memory tx buffer.\n",txbuf_size >> pagebits);
       fprintf(stderr, "mapping %d page(s) for shared memory rx buffer.\n",rxbuf_size >> pagebits);
       fprintf(stderr, "handling signal %d: tx data from parent.\n",txsig);
       fprintf(stderr, "handling signal %d: rx data ACK from parent.\n",rxsig);
       fprintf(stderr, "raising signal %d: tx data ACK to parent.\n",txsig);
       fprintf(stderr, "raising signal %d: rx data for parent.\n",rxsig);
     }
   }
   pstrcpy(ifname, IFNAMELENGTH, device);
   if (verbose > 3) {
      fprintf(stderr,"interface name is %s\n",device);
   }
   if (rstderr != NULL) {
     if (verbose > 3) {
       fprintf(stderr, "redirecting stderr to %s.\n",rstderr);
     } 
     if ((fderr = open(rstderr,O_WRONLY|O_CREAT|O_TRUNC,S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH)) == -1) {
       fprintf(stderr,"Failed to open %s (%s)\n",rstderr,strerror(errno));
       exit(EXIT_FAILURE);
     }
     if (close(2)==-1) {
         fprintf(stderr,"Close of stderr failed (%s)\n",strerror(errno));
         exit(EXIT_FAILURE);
     }
     if (dup2(fderr,2) == -1) {
         fprintf(stderr,"Failed to duplicate stderr %s (%s)\n",rstderr,strerror(errno));
         exit(EXIT_FAILURE);
     }
   }
#ifdef LINUX
   mode = iffmt | ifmode;
   if (verbose > 4) {
     fprintf(stderr, "mode=%s,format=%s.\n",((mode & IFF_TAP) == 0) ? "TUN" : "TAP",
                                            ((mode & IFF_NO_PI) == 0)? "PI" : "NO_PI");
   }
#endif
   if (bufshmname != NULL || shmname != NULL) {
      if (bufshmname != NULL) {
        pstrcpy (txshmname,MAXNAME-2,bufshmname);
        pstrcpy (rxshmname,MAXNAME-2,bufshmname);
      }
      if (shmname != NULL) {
        pstrcpy (txshmname,MAXNAME-2,shmname);
        pstrcpy (rxshmname,MAXNAME-2,shmname);
      }
      pstrcpy(txshmname+strlen(txshmname),MAXNAME-strlen(txshmname)-1,"0");
      pstrcpy(rxshmname+strlen(rxshmname),MAXNAME-strlen(rxshmname)-1,"1");
   }
   if (shmname == NULL) {
      if (bufshmname == NULL) {
         if (verbose > 4) {
            fprintf(stderr, "allocating buffers [tx %d,rx %d].\n",txbuf_size,rxbuf_size);
         }
         txbuf = allocbuf(txbuf_size);
         rxbuf = allocbuf(rxbuf_size);
      } else {
         if (verbose > 4) {
           fprintf(stderr, "creating \"shared\" memory buffers [%s %d,%s %d].\n",
                              txshmname, txbuf_size,rxshmname,rxbuf_size);
         }
         create_shared_mem(txshmname, txbuf_size, O_RDWR,S_IRUSR|S_IWUSR);
         create_shared_mem(rxshmname, rxbuf_size, O_RDWR,S_IRUSR|S_IWUSR);
         if (verbose > 4) {
           fprintf(stderr, "mapping buffers [tx %d,rx %d].\n",txbuf_size,rxbuf_size);
         }
         txbuf = mapbuf(NULL,txshmname,txbuf_size,
                  O_RDWR,S_IRUSR|S_IWUSR,MAP_SHARED,PROT_READ|PROT_WRITE);
         rxbuf = mapbuf(NULL,rxshmname,rxbuf_size,
                  O_RDWR,S_IRUSR|S_IWUSR,MAP_SHARED,PROT_READ|PROT_WRITE);
         if (verbose > 4) {
            fprintf(stderr, "buffers mapped.\n");
         } 
      }
   } else {
      if (verbose > 4) {
        fprintf(stderr, "mapping buffers [tx %d,rx %d].\n",txbuf_size,rxbuf_size);
      }
      txbuf = mapbuf(NULL,txshmname,txbuf_size,
              O_RDONLY,S_IRUSR,MAP_SHARED,PROT_READ);
      rxbuf = mapbuf(NULL,rxshmname,rxbuf_size,
              O_RDWR,S_IRUSR|S_IWUSR,MAP_SHARED,PROT_READ|PROT_WRITE);
      if (verbose > 4) {
         fprintf(stderr, "buffers mapped.\n");
      } 
   }
   if (verbose > 4) {
      fprintf(stderr, "initialising interface.\n");
   }
   raise_privs(uid,euid);
#ifdef LINUX
   fd = tap_init(ifname, IFNAMELENGTH, mode);

   if (fd == -1) {
         drop_privs(PERMANENTLY,uid,euid);
         fprintf(stderr, "initialization of %s failed.\n",ifname);
         exit(EXIT_FAILURE);
   }
   if (euid != uid) {
     if (elevprivs_up == 0) {
        if (elevprivs_down == 0) {
           drop_privs(PERMANENTLY,uid,euid);
        } else {
           drop_privs(TEMPORARILY,uid,euid);
        }
     }
   }
   if (up && up[0] != '\0' && strcmp(up, "none") != 0) {
      if (euid != uid) {
         if (elevprivs_up != 0) {
            if (check_script_perms(up) != 0) {
                 fprintf(stderr,"can't exec script %s\n",up);
                 close(fd);
                 exit(EXIT_FAILURE);
            }
         }
      }
      if (launch_script(up, ifname)) {
         fprintf(stderr, "%s i/f initialization script failed.\n",ifname);
         close(fd);
         exit(EXIT_FAILURE);
      }
   }
   if (euid != uid) {
     if (elevprivs_down == 0) {
        drop_privs(PERMANENTLY,uid,euid);
     } else {
        drop_privs(TEMPORARILY,uid,euid);
     }
   }
#endif
#ifdef OPENBSD
   if (euid != uid) {
     if (elevprivs_up == 0) {
        if (elevprivs_down == 0) {
           drop_privs(PERMANENTLY,uid,euid);
        } else {
           drop_privs(TEMPORARILY,uid,euid);
        }
     }
   }
   if (up && up[0] != '\0' && strcmp(up, "none") != 0) {
      if (euid != uid) {
         if (elevprivs_up != 0) {
            if (check_script_perms(up) != 0) {
                 fprintf(stderr,"can't exec script %s\n",up);
                 close(fd);
                 exit(EXIT_FAILURE);
            }
         }
      }
      if (launch_script(up, ifname)) {
         fprintf(stderr, "%s i/f initialization script failed.\n",ifname);
         close(fd);
         exit(EXIT_FAILURE);
      }
   }
   fd = tap_init(ifname);

   if (fd == -1) {
         drop_privs(PERMANENTLY,uid,euid);
         fprintf(stderr, "initialization of %s failed.\n",ifname);
         exit(EXIT_FAILURE);
   }
   if (euid != uid) {
     if (elevprivs_down == 0) {
        drop_privs(PERMANENTLY,uid,euid);
     } else {
        drop_privs(TEMPORARILY,uid,euid);
     }
   }
#endif
   if (verbose > 4) {
      fprintf(stderr, "initialization succeeded.\n");
   }
   if (shmname == NULL) {
      if (bufshmname == NULL) {
         if (verbose > 4) {
            fprintf(stderr, "do_io: starting in FDBUFF mode.\n");
         }
         ret = do_io(to,ppid,FDBUFF,0,1,txbuf,txbuf_size,fd,rxbuf,rxbuf_size);
      } else {
         if (verbose > 4) {
            fprintf(stderr, "do_io: starting in SHMBUFF mode.\n");
         }
         ret = do_io(to,ppid,SHMBUFF,0,1,txbuf,txbuf_size,fd,rxbuf,rxbuf_size);
      }
   } else {
      if (verbose > 4) {
         fprintf(stderr, "do_io: starting in SHMBUFF|SIGNALIO mode.\n");
      }
      ret = do_io(to,ppid,SHMBUFF|SIGNALIO,-1,-1,txbuf,txbuf_size,fd,rxbuf,rxbuf_size);
   }
   if (ret < 0) {
       fprintf(stderr, "do_io: failed %d.\n",ret);
   } else
       if (verbose > 4) {
          fprintf(stderr, "do_io: returned OK.\n");
       }
   if (verbose > 4) {
      fprintf(stderr, "shutting down interface.\n");
   }
   if (elevprivs_down != 0) {
     raise_privs(uid,euid);
   }
   if (tap_shutdown(down, ifname, fd) != 0) {
     fprintf(stderr, "shutdown failed.\n");
     ret = -1;
   }
   if (elevprivs_down != 0)
      drop_privs(PERMANENTLY,uid,euid);

   if (ret < 0) 
      exit(EXIT_FAILURE);
   else
      exit(EXIT_SUCCESS);
}
