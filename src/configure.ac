#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([redoctober], [2.2], [ian.a.n.grant@googlemail.com])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([runtime/main.c])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_HEADERS([runtime/autoconfig.h])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])
# AM_SILENT_RULES([yes])

AC_ARG_ENABLE([manual],
  [  --enable-manual         produce human-readable Makefiles for manual configuration],
  [case "${enableval}" in
       yes) manual=true ;;
       no)  manual=false ;;
       *) AC_MSG_ERROR([bad value ${enableval} for --enable-manual]) ;;
   esac],[manual=false])

if test x$manual = xtrue ; then
   AC_MSG_NOTICE([Manual configuration selected])
   AC_MSG_NOTICE([Creating Makefile])
      cp Makefile.manual Makefile
   AC_MSG_NOTICE([Creating Makefile.inc])
      cp Makefile.inc.manual Makefile.inc
   AC_MSG_NOTICE([Creating runtime/Makefile])
      cp runtime/Makefile.manual runtime/Makefile
   AC_MSG_NOTICE([Creating compiler/Makefile])
      cp compiler/Makefile.manual compiler/Makefile
   AC_MSG_NOTICE([Creating mosmllib/Makefile])
      cp mosmllib/Makefile.manual mosmllib/Makefile
   AC_MSG_NOTICE([Creating launch/Makefile])
      cp launch/Makefile.manual launch/Makefile
   exit 0   
fi

AC_ARG_ENABLE([debug],
  [  --enable-debug          also build a debugging camlrunm],
  [case "${enableval}" in
       yes) debug=true ;;
       no)  debug=false ;;
       *) AC_MSG_ERROR([bad value ${enableval} for --enable-debug]) ;;
   esac],[debug=false])
AM_CONDITIONAL([DEBUG], [test x$debug = xtrue])

AC_ARG_ENABLE([cross-win32],
  [  --enable-cross-win32    cross-compile to 32 bit Windows. Doesn't do anything. FIXME],
  [case "${enableval}" in
       yes) crossw32=true ;;
       no)  crossw32=false ;;
       *) AC_MSG_ERROR([bad value ${enableval} for --enable-cross-w32]) ;;
   esac],[crossw32=false])
AM_CONDITIONAL([CROSSW32], [test x$crossw32 = xtrue])

AC_MSG_CHECKING([run path])
AC_ARG_WITH([run-path],
  [[  --with-run-path[=DIR]   add DIR as another place where CAML runtime
                          should look for dynamic libraries [EPREFIX/lib]]],
  [runpath=$withval],
  [runpath=$withval])

if test "x$runpath" = "xno" ; then
   runpath=""
fi
if test "x$runpath" = "xyes" ; then
   runpath=${prefix}/lib
fi
if test "x$runpath" != "x" ; then
   AC_MSG_RESULT($runpath)
   CAMLRTLDFLAGS="-Wl,-rpath,$runpath"
else
   AC_MSG_RESULT([none])
fi
AC_SUBST([CAMLRTLDFLAGS])

# Check host type
AC_CANONICAL_HOST

AC_MSG_CHECKING([OS])
if test "$host_os" = "linux-gnu" ; then
   AC_DEFINE([linux],[1], [Define to 1 if host OS is linux.])
fi
AC_MSG_RESULT([$host_os])
AC_SUBST(linux)

# Checks for programs.
AC_PROG_SED
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

# Checks for libtool.
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL
AM_CONDITIONAL([HAVE_SHARED_LIBRARIES], [test "x$enable_shared" = "xyes"])

# Checks for libraries.
AC_CHECK_LIB([dl],  [dlopen])
AC_CHECK_LIB([c],  [fgetc])
AC_CHECK_LIB([m],  [sin])

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h fcntl.h float.h limits.h memory.h     \
                  netinet/in.h stddef.h stdint.h stdlib.h strings.h \ 
                  string.h sys/param.h sys/socket.h sys/time.h      \
                  sys/timeb.h unistd.h utime.h dirent.h termios.h   \
                  signal.h setjmp.h dlfcn.h])

         if test "x$ac_cv_lib_dl_dlopen" = "xyes" \
           -a "x$ac_cv_header_dlfcn_h" = "xyes" ; then
              AC_CHECK_DECL([RTLD_DEEPBIND],
                [AC_DEFINE([HAS_RTLD_DEEPBIND],[1],
                           [Define to 1 if HAS_RTLD_DEEPBIND is defined.])],
                [],
                  [[#include <dlfcn.h>
                ]])
              AC_CHECK_DECL([RTLD_NODELETE],
                [AC_DEFINE([HAS_RTLD_NODELETE],[1],
                           [Define to 1 if HAS_RTLD_NODELETE is defined.])],
                [],
                  [[#include <dlfcn.h>
                ]])
              AC_CHECK_DECL([RTLD_NOLOAD],
                [AC_DEFINE([HAS_RTLD_NOLOAD],[1],
                           [Define to 1 if HAS_RTLD_NOLOAD is defined.])],
                [],
                  [[#include <dlfcn.h>
                ]])
         fi

AC_SUBST(HAS_RTLD_DEEPBIND)
AC_SUBST(HAS_RTLD_NODELETE)
AC_SUBST(HAS_RTLD_NOLOAD)

dnl We require the pkg.m4 set of macros from pkg-config.
dnl Make sure it's available.
m4_pattern_forbid([PKG_CHECK_MODULES])

AC_LANG([C]) 

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T
AC_C_CHAR_UNSIGNED

if test "$__CHAR_UNSIGNED__" = "1" ; then
   AC_MSG_CHECKING([to see if that matters])
   AC_RUN_IFELSE(
     [AC_LANG_PROGRAM([[signed char foo[]="\377";]],
                      [[int i;i = foo[0];exit(i != -1);]])],
     [AC_DEFINE([SIGNED_CHAR_WORKS],[1],
         [C compiler char is unsigned, but signed char type works.])
      AC_MSG_RESULT([no])],
     [AC_MSG_RESULT([yes])
      AC_MSG_ERROR([Sorry, chars are unsigned and we don't know what to do about this.])],
     [AC_MSG_FAILURE])
fi
AC_SUBST(SIGNED_CHAR_WORKS)

AC_C_VOLATILE

AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([long *])
AC_CHECK_ALIGNOF([long])
AC_CHECK_ALIGNOF([double])
if test "$ALIGNOF_LONG" != "$ALIGNOF_DOUBLE" ; then
   AC_DEFINE([ALIGN_DOUBLE],[1],[Define to 1 if doubles must be double-word aligned.])   
fi
AC_SUBST(ALIGN_DOUBLE)

AC_MSG_CHECKING([machine word size])
if test "$SIZEOF_INT:$SIZEOF_LONG::$SIZEOF_LONG_P" = "4:8:8" ; then
   AC_DEFINE([SIXTYFOUR],[1],[Define to 1 if this is a 64 bit architecture CPU.])
fi
if test "$SIZEOF_INT:$SIZEOF_LONG::$SIZEOF_LONG_P" = "8:8:8" ; then
   AC_MSG_ERROR([Integers are 8 bytes. CAML runtime can't be guaranteed to work. Sorry.])
fi
if test "$SIZEOF_LONG_P" != "$SIZEOF_LONG" ; then
   AC_MSG_ERROR([sizeof(long) != sizeof(long *). CAML runtime won't work. Sorry.])
fi
if test -z "$SIXTYFOUR" ; then
   AC_MSG_RESULT([32 bit]) ; else
   AC_MSG_RESULT([64 bit])
fi
AC_SUBST(SIXTYFOUR)

AC_C_BIGENDIAN(
   [AC_DEFINE([MOSML_BIG_ENDIAN],[1],
              [Define to 1 if the C compiler word order is big-endian.])])
AC_SUBST(MOSML_BIG_ENDIAN)

# Checks for library functions.

AC_CHECK_DECLS([memmove])

if test "x$ac_cv_have_decl_memmove" = "xyes" ; then
   AC_CHECK_FUNCS([memmove])
fi

if test "x$ac_cv_func_memmove" = "xyes" ; then
   AC_MSG_CHECKING([memmove semantics])
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
#include <stdlib.h>
#if HAVE_STRING_H
#include <string.h>
#endif
char buffer[27];
#define cpy(s1,s2,n) memmove(s2,s1,n)
]], [[
  cpy("abcdefghijklmnopqrstuvwxyz", buffer, 27);
  if (strcmp(buffer, "abcdefghijklmnopqrstuvwxyz") != 0) exit(1);
  cpy(buffer, buffer+3, 26-3);
  if (strcmp(buffer, "abcabcdefghijklmnopqrstuvw") != 0) exit(1);
  cpy("abcdefghijklmnopqrstuvwxyz", buffer, 27);
  cpy(buffer+3, buffer, 26-3);
  if (strcmp(buffer, "defghijklmnopqrstuvwxyzxyz") != 0) exit(1);
  exit(0);
]])],
  [AC_DEFINE([HAS_MEMMOVE],[1],
             [Define to 1 if memmove handles overlapping src/dest correctly.])
   AC_MSG_RESULT([OK])],
  [AC_MSG_RESULT([Failed])])
fi
AC_SUBST(HAS_MEMMOVE)

dnl It would be nice to abstract this common test some time ...
AC_CHECK_DECLS([bcopy])

if test "x$ac_cv_have_decl_bcopy" = "xyes" ; then
   AC_CHECK_FUNCS([bcopy])
fi

if test "x$ac_cv_func_bcopy" = "xyes" ; then
   AC_MSG_CHECKING([bcopy semantics])
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
#include <stdlib.h>
#if HAVE_STRINGS_H
#include <strings.h>
#endif
char buffer[27];
#define cpy(s1,s2,n) bcopy(s1,s2,n)
]], [[
  cpy("abcdefghijklmnopqrstuvwxyz", buffer, 27);
  if (strcmp(buffer, "abcdefghijklmnopqrstuvwxyz") != 0) exit(1);
  cpy(buffer, buffer+3, 26-3);
  if (strcmp(buffer, "abcabcdefghijklmnopqrstuvw") != 0) exit(1);
  cpy("abcdefghijklmnopqrstuvwxyz", buffer, 27);
  cpy(buffer+3, buffer, 26-3);
  if (strcmp(buffer, "defghijklmnopqrstuvwxyzxyz") != 0) exit(1);
  exit(0);
]])],
  [AC_DEFINE([HAS_BCOPY],[1],
             [Define to 1 if bcopy handles overlapping src/dest correctly.])
   AC_MSG_RESULT([OK])],
  [AC_MSG_RESULT([Failed])])
fi
AC_SUBST(HAS_BCOPY)

dnl It would be nice to abstract this common test some time ...
AC_CHECK_DECLS([memcpy])

if test "x$ac_cv_have_decl_memcpy" = "xyes" ; then
   AC_CHECK_FUNCS([memcpy])
fi

if test "x$ac_cv_func_memcpy" = "xyes" ; then
   AC_MSG_CHECKING([memcpy semantics])
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
#include <stdlib.h>
#if HAVE_STRING_H
#include <string.h>
#endif
char buffer[27];
#define cpy(s1,s2,n) memcpy(s2,s1,n)
]], [[
  cpy("abcdefghijklmnopqrstuvwxyz", buffer, 27);
  if (strcmp(buffer, "abcdefghijklmnopqrstuvwxyz") != 0) exit(1);
  cpy(buffer, buffer+3, 26-3);
  if (strcmp(buffer, "abcabcdefghijklmnopqrstuvw") != 0) exit(1);
  cpy("abcdefghijklmnopqrstuvwxyz", buffer, 27);
  cpy(buffer+3, buffer, 26-3);
  if (strcmp(buffer, "defghijklmnopqrstuvwxyzxyz") != 0) exit(1);
  exit(0);
]])],
  [AC_DEFINE([HAS_MEMCPY],[1],
             [Define to 1 if memcpy handles overlapping src/dest correctly.])
   AC_MSG_RESULT([OK!])],
  [AC_MSG_RESULT([Failed])])
fi
AC_SUBST(HAS_MEMCPY)

AC_FUNC_FORK
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_FUNC_REALLOC

AC_CHECK_FUNCS([alarm dup2 floor ftime getcwd getpagesize gettimeofday \
                inet_ntoa memset mkdir pow realpath regcomp rint       \
                rmdir select sqrt strchr strerror rename strtoul uname \
                utime _setjmp socket socketpair bind             \
                listen accept connect lockf mkfifo getpriority         \
                setpriority utimes fchmod fchown truncate ftruncate    \
                symlink readlink lstat wait3 waitpid])

AC_MSG_CHECKING([signals semantics])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
#include <stdlib.h>
#if HAVE_SETJMP_H
#include <setjmp.h>
#endif
#if HAVE_SIGNAL_H
#include <signal.h>
#endif
int counter;
void sig_handler(dummy) int dummy;{counter++;}
]], [[
  signal(SIGINT, sig_handler);
  counter = 0;
  kill(getpid(), SIGINT);
  kill(getpid(), SIGINT);
  return (counter == 2 ? 0 : 1);
]])],
  [AC_DEFINE([BSD_SIGNALS],[1],
             [Define to 1 if signals have BSD semantics.])
   AC_MSG_RESULT([BSD])],
  [AC_MSG_RESULT([System V])])

AC_CHECK_TYPES([__sighandler_t],
   [AC_DEFINE([HAS_SIGHANDLER_T],[1],
              [Define to 1 if has __sighandler_t typedef.])],
   [],
   [[#include <signal.h>
   ]])
AC_SUBST(HAS_SIGHANDLER_T)

AC_MSG_CHECKING([word access])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <stdlib.h>
#include <stdio.h>
#if HAVE_SIGNAL_H
#include <signal.h>
#endif
#if HAVE_SETJMP_H
#include <setjmp.h>
#endif
long foo;

void access16(p)
     short * p;
{
  foo = *p;
}

void access32(p)
     long * p;
{
  foo = *p;
}

jmp_buf failure;

void sig_handler(dummy)
     int dummy;
{
  longjmp(failure, 1);
}

int test(fct, p)
     void (*fct)();
     char * p;
{
  int res;

  signal(SIGSEGV, sig_handler);
  signal(SIGBUS, sig_handler);
  if(setjmp(failure) == 0) {
    fct(p);
    res = 0;
  } else {
    res = 1;
  }
  signal(SIGSEGV, SIG_DFL);
  signal(SIGBUS, SIG_DFL);
  return res;
}

jmp_buf timer;

void alarm_handler(dummy)
     int dummy;
{
  longjmp(timer, 1);
}

void use(n)
     int n;
{
  return;
}

int speedtest(p)
     char * p;
{
  int * q;
  volatile int total;
  int i;
  volatile int sum;

#ifdef linux
  /* Added by sestoft@dina.kvl.dk to make this work under REDHAT 5.0 */
  struct sigaction sigact;
  sigset_t emptyset;
  sigemptyset(&emptyset);
  sigact.sa_handler  = &alarm_handler;
  sigact.sa_mask     = emptyset;
  sigact.sa_flags    = SA_NODEFER;
  sigaction(SIGALRM, &sigact, 0); 
#else
  signal(SIGALRM, alarm_handler);
#endif
  sum = 0;
  if (setjmp(timer) == 0) {
    alarm(1);
    total = 0;
    while(1) {
      for (q = (int *) p, i = 1000; i > 0; q++, i--)
        sum += *q;
      total++;
    }
  }
  use(sum);
  signal(SIGALRM, SIG_DFL);
  return total;
}
]], [[
  long n[1001];
  int speed_aligned, speed_unaligned;

  if (test(access16, (char *) n + 1)) exit(1);
  if (test(access32, (char *) n + 1)) exit(1);
  if (test(access32, (char *) n + 2)) exit(1);
  speed_aligned = speedtest((char *) n);
  speed_unaligned = speedtest((char *) n + 1);
  if (speed_aligned >= 3 * speed_unaligned) exit(1);
  exit(0);
]])],
  [AC_MSG_RESULT([un-aligned is OK])],
  [AC_DEFINE([ALIGNMENT],[1],
             [Define to 1 if aligned word access is quicker than non-aligned.])
   AC_MSG_RESULT([at least 3 times quicker when aligned])])
AC_SUBST(ALIGNMENT)

if test "$ac_cv_func__setjmp" = "yes" ; then
   AC_DEFINE([HAS__SETJMP],[1],
             [Define to 1 if we have _setjmp call.])
fi
AC_SUBST(HAS_SETJMP)

if test "$ac_cv_func_rename" = "yes" ; then
   AC_DEFINE([HAS_RENAME],[1],
             [Define to 1 if we have rename call.])
fi
AC_SUBST(HAS_RENAME)

if test "$ac_cv_func_strerror" = "yes" ; then
   AC_DEFINE([HAS_STRERROR],[1],
             [Define to 1 if we have strerror call.])
fi
AC_SUBST(HAS_STRERROR)

if test "$ac_cv_header_unistd_h" = "yes" ; then
   AC_DEFINE([HAS_UNISTD],[1],
             [Define to 1 if we have <unistd.h> header.])
fi
AC_SUBST(HAS_UNISTD)

if test "$ac_cv_header_dirent_h" = "yes" ; then
   AC_DEFINE([HAS_DIRENT],[1],
             [Define to 1 if we have <dirent.h> header.])
fi
AC_SUBST(HAS_DIRENT)

if test "$ac_cv_func_lockf" = "yes" ; then
   AC_DEFINE([HAS_LOCKF],[1],
             [Define to 1 if we have the lockf call.])
fi
AC_SUBST(HAS_LOCKF)

if test "$ac_cv_func_mkfifo" = "yes" ; then
   AC_DEFINE([HAS_MKFIFO],[1],
             [Define to 1 if we have the mkfifo call.])
fi
AC_SUBST(HAS_MKFIFO)

if test "$ac_cv_func_getpriority" = "yes" -a \
        "$ac_cv_func_setpriority" = "yes" ; then
   AC_DEFINE([HAS_GETPRIORITY],[1],
             [Define to 1 if we have get/setpriority calls.])
fi
AC_SUBST(HAS_GETPRIORITY)

if test "$ac_cv_func_utime" = "yes" ; then
   AC_DEFINE([HAS_UTIME],[1],
             [Define to 1 if we have the utime call.])
fi
AC_SUBST(HAS_UTIME)

if test "$ac_cv_func_utimes" = "yes" ; then
   AC_DEFINE([HAS_UTIMES],[1],
             [Define to 1 if we have the utimes call.])
fi
AC_SUBST(HAS_UTIMES)

if test "$ac_cv_func_dup2" = "yes" ; then
   AC_DEFINE([HAS_DUP2],[1],
             [Define to 1 if we have the dup2 call.])
fi
AC_SUBST(HAS_DUP2)

if test "$ac_cv_func_fchmod" = "yes" ; then
   AC_DEFINE([HAS_FCHMOD],[1],
             [Define to 1 if we have the fchmod call.])
fi
AC_SUBST(HAS_FCHMOD)

if test "$ac_cv_func_truncate" = "yes" ; then
   AC_DEFINE([HAS_TRUNCATE],[1],
             [Define to 1 if we have the truncate call.])
fi
AC_SUBST(HAS_TRUNCATE)

if test "$ac_cv_func_select" = "yes" ; then
   AC_DEFINE([HAS_SELECT],[1],
             [Define to 1 if we have the select call.])
fi
AC_SUBST(HAS_SELECT)

if test "$ac_cv_func_symlink" = "yes" ; then
   AC_DEFINE([HAS_SYMLINK],[1],
             [Define to 1 if we have the symlink call.])
fi
AC_SUBST(HAS_SYMLINK)

if test "$ac_cv_func_wait3" = "yes" ; then
   AC_DEFINE([HAS_WAIT3],[1],
             [Define to 1 if we have the wait3 call.])
fi
AC_SUBST(HAS_WAIT3)

if test "$ac_cv_func_waitpid" = "yes" ; then
   AC_DEFINE([HAS_WAITPID],[1],
             [Define to 1 if we have the waitpid call.])
fi
AC_SUBST(HAS_WAITPID)

# On the basis of all the following, we decide which optional
# sub-directories to build

AC_ARG_ENABLE(nothing,
  [  --enable-nothing        do not build any optional components],
  enable_nothing=yes,)
AM_CONDITIONAL([NOTHING],[test x$enable_nothing = xyes])

AC_ARG_ENABLE(unix,
  [  --disable-unix          do not build the Unix system call interface],,
  enable_unix=yes)
AM_CONDITIONAL([UNIX],[test x$enable_unix = xyes])

AC_ARG_ENABLE(sockets,
  [  --disable-sockets       do not build the BSD sockets system call interface],,
  enable_sockets=yes)
AM_CONDITIONAL([SOCKETS],[test x$enable_sockets = xyes])

AC_ARG_ENABLE(crypt,
  [  --disable-crypt         do not build the Unix crypt system call interface],,
  enable_crypt=yes)
AM_CONDITIONAL([CRYPT],[test x$enable_crypt = xyes])

AC_ARG_ENABLE(regex,
  [  --disable-regex         do not build the Posix regular expressions interface],,
  enable_regex=yes)
AM_CONDITIONAL([REGEX],[test x$enable_regex = xyes])

AC_ARG_ENABLE(ffi,
  [  --disable-ffi           do not build the GNU FFI (foreign functions) interface],,
  enable_ffi=yes)

AC_ARG_ENABLE(jit,
  [  --disable-jit           do not build the GNU Lightning (just-in-time assembler) interface],,
  enable_jit=yes)
AM_CONDITIONAL([JIT],[test x$enable_jit = xyes])

AC_ARG_ENABLE(postgresql,
  [  --disable-postgresql    do not build the PostgreSQL database (libpq client) interface],,
  enable_postgresql=yes)
AM_CONDITIONAL([POSTGRESQL],[test x$enable_postgresql = xyes])

AC_ARG_ENABLE(intinf,
  [  --disable-intinf        do not build the IntInf arbitrary precision arithmetic (GNU MP) interface],,
  enable_intinf=yes)
AM_CONDITIONAL([INTINF],[test x$enable_intinf = xyes])

AC_ARG_ENABLE(guilert,
  [  --disable-guilert       do not build the GNU Guile run-time interface],,
  enable_guilert=yes)
AM_CONDITIONAL([GUILERT],[test x$enable_guilert = xyes])

# Now whatever hasn't been disallowed, we try ... but first:

PKG_CHECK_EXISTS([guile-2.0],[],[])

dnl We have to do this once, otherwise the macro never gets
dnl instantiated if the first instance was under an unsatisfied
dnl condition. Really, this is a terrible 'language'. It's not a
dnl language at all, really, it's just alphabet soup!

if test x$enable_nothing != xyes ; then
   if test x$enable_guilert = xyes ; then
      PKG_CHECK_EXISTS([guile-2.0],
         [AC_DEFINE([HAVE_GUILE],[1],
                    [Define to 1 if we have libguile.])
          PKG_CHECK_MODULES([GUILE], [guile-2.0 >= 2.0.11])],
         [])
      AC_SUBST(HAVE_GUILE)
   fi
   if test x$enable_ffi = xyes ; then
      PKG_CHECK_EXISTS([libffi],
         [AC_DEFINE([HAVE_LIBFFI],[1],
                    [Define to 1 if we have libffi.])
          PKG_CHECK_MODULES([LIBFFI],[libffi >= 3.0.13],
                            [AC_LIB_LINKFLAGS([ffi],
                                              [],
                                              [#include <ffi.h>],
                                              [ffi_prep_cif(NULL, 0, 0, NULL, NULL);])
                             if test "x$HAVE_LIBFFI" != "xyes"; then
                                AC_MSG_ERROR([pkg-config indicates we have libffi 3.0.13 or greater, with
       LIBFFI_CFLAGS=$LIBFFI_CFLAGS
   and LIBFFI_LIBS=$LIBFFI_LIBS
   but it doesn't seem to work. See config.log for details. Try explicitly setting the
   LIBFFI_LIBS or LIBFFI_CFLAGS argument variables to the correct values. If that doesn't
   work, then you can try to build libffi from source. As a last resort, you could disable
   mosml libffi functionality entirely using the ./configure command-line option --disable-ffi.])
                             else
                                AC_DEFINE([HAVE_LIBFFI],[1],
                                          [Define to 1 if we have GNU FFI >= 3.0.13.])
                             fi
                             save_CPPFLAGS="$CPPFLAGS"
                             if test "x$LIBFFI_CFLAGS" != "x"; then
                                CPPFLAGS="$LIBFFI_CFLAGS $CPPFLAGS"
                             fi
                             AC_CHECK_DECLS([ffi_prep_closure_loc], [], [], [[#include <ffi.h>]])
                             CPPFLAGS="$save_CPPFLAGS"],
                           [enable_ffi=no])],
                           [])
      AC_SUBST(HAVE_LIBFFI)
      AM_CONDITIONAL([FFI],[test x$enable_ffi = xyes])
   fi
   if test x$enable_postgresql = xyes ; then
      PKG_CHECK_EXISTS([libpq],
         [AC_DEFINE([HAVE_LIBPQ],[1],
                    [Define to 1 if we have libpq.])
          PKG_CHECK_MODULES([LIBPQ], [libpq >= 9.3.4])],
         [])
      AC_SUBST(HAVE_LIBPQ)
      AC_ARG_VAR([PGCTL],[The PostgreSQL pg_ctl server controller program binary])
      AC_PATH_PROG([PGCTL],pg_ctl,,)
      AC_ARG_VAR([PSQL],[The PostgreSQL psql SQL client program binary])
      AC_PATH_PROG([PSQL],psql,,)
   fi
   if test x$enable_intinf = xyes ; then
      dnl GMP tests (whipped from Guile's confiure.ac)
      AC_LIB_LINKFLAGS([gmp],
        [],
        [#include <gmp.h>],
        [mpz_import (0, 0, 0, 0, 0, 0, 0);])
   
      if test "x$HAVE_LIBGMP" != "xyes"; then
        AC_MSG_ERROR([GNU MP 4.1 or greater not found, see README])
      else
        AC_DEFINE([HAVE_LIBGMP],[1],
                  [Define to 1 if we have GNU MP >= 4.1.])
      fi
      AC_SUBST(HAVE_LIBGMP)
   
      dnl `mpz_inits' and `mpz_clears' appeared in GMP 5.0.0.
      save_CPPFLAGS="$CPPFLAGS"
      if test "x$LIBGMP_PREFIX" != "x"; then
         CPPFLAGS="-I$LIBGMP_PREFIX $CPPFLAGS"
      fi
      AC_CHECK_DECLS([mpz_inits], [], [], [[#include <gmp.h>]])
      CPPFLAGS="$save_CPPFLAGS"
   fi
   if test x$enable_jit = xyes ; then
      dnl Lightning tests - same pattern
      AC_LIB_LINKFLAGS([lightning],
        [],
        [#include <lightning.h>],
        [_jit_get_data(NULL, NULL, NULL);])
      if test "x$HAVE_LIBLIGHTNING" != "xyes"; then
        AC_MSG_ERROR([GNU Lightning 2.0.4 or greater not found, see README])
      else
        AC_DEFINE([HAVE_LIBLIGHTNING],[1],
                  [Define to 1 if we have GNU Lightning >= 2.0.4.])
      fi
      AC_SUBST(HAVE_LIBLIGHTNING)
      save_CPPFLAGS="$CPPFLAGS"
      if test "x$LIBLIGHTNING_PREFIX" != "x"; then
         CPPFLAGS="-I$LIBLIGHTNING_PREFIX $CPPFLAGS"
      fi
      AC_CHECK_DECLS([_jit_get_data], [], [], [[#include <lightning.h>]])
      CPPFLAGS="$save_CPPFLAGS"
   fi
   if test x$enable_sockets = xyes ; then
      if test "$ac_cv_func_socket" = "yes" -a \
              "$ac_cv_func_socketpair" = "yes" -a \
              "$ac_cv_func_bind" = "yes" -a \
              "$ac_cv_func_listen" = "yes" -a \
              "$ac_cv_func_accept" = "yes" -a \
              "$ac_cv_func_connect" = "yes" ; then
         AC_DEFINE([HAS_SOCKETS],[1],
          [Define to 1 if we have BSD sockets (sockets, socketpair, bind, listen, etc.)])
      fi
      AC_SUBST(HAS_SOCKETS)
   fi
   if test x$enable_crypt = xyes ; then
      AC_SEARCH_LIBS(crypt, crypt,
         [AC_DEFINE([HAS_CRYPT],1,
                    [Define to 1 if we have the crypt system call.])])
      AC_SUBST(HAS_CRYPT)
   fi
   if test x$enable_unix = xyes ; then
      if test "x$ac_cv_header_sys_param_h" = "xyes" ; then
         AC_CHECK_FUNCS([getgroups])
         if test "x$ac_cv_func_getgroups" = "xyes" ; then
            AC_CHECK_DECL([NGROUPS],
              [AC_DEFINE([HAS_GETGROUPS],[1],
                         [Define to 1 if have getgroups and NGROUPS is defined.])],
              [],
              [[#include <sys/param.h>
              ]])
         fi
      fi
      AC_SUBST(HAS_GETGROUPS)
   
      AC_SYS_POSIX_TERMIOS
      if test "$ac_cv_sys_posix_termios" = "yes" ; then
         AC_DEFINE([HAS_TERMIOS],[1],
                   [Define to 1 if we have Posix termios routines.])
      fi
      AC_SUBST(HAS_TERMIOS)
   fi
fi # !enable_nothing

AC_SUBST([prefix]) AC_SUBST([libdir])

AC_OUTPUT([Makefile \
           runtime/Makefile \
           Makefile.inc])

if test -f runtime/m.h ; then
   rm -f runtime/m.h
fi
AC_MSG_NOTICE([creating runtime/m.h])
echo "#include \"autoconfig.h\"" >> runtime/m.h  

if test -f runtime/s.h ; then
   rm -f runtime/s.h
fi
AC_MSG_NOTICE([creating runtime/s.h])
echo "#include \"autoconfig.h\"" >> runtime/s.h  
