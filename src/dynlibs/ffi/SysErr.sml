datatype syserr =
    E2BIG
  | EACCES
  | EADDRINUSE
  | EADDRNOTAVAIL
  | EADV
  | EAFNOSUPPORT
  | EAGAIN
  | EALREADY
  | EBADE
  | EBADF
  | EBADFD
  | EBADMSG
  | EBADR
  | EBADRQC
  | EBADSLT
  | EBFONT
  | EBUSY
  | ECANCELED
  | ECHILD
  | ECHRNG
  | ECOMM
  | ECONNABORTED
  | ECONNREFUSED
  | ECONNRESET
  | EDEADLK
  | EDEADLOCK
  | EDESTADDRREQ
  | EDOM
  | EDOTDOT
  | EDQUOT
  | EEXIST
  | EFAULT
  | EFBIG
  | EHOSTDOWN
  | EHOSTUNREACH
  | EIDRM
  | EILSEQ
  | EINPROGRESS
  | EINTR
  | EINVAL
  | EIO
  | EISCONN
  | EISDIR
  | EISNAM
  | EKEYEXPIRED
  | EKEYREJECTED
  | EKEYREVOKED
  | EL2HLT
  | EL2NSYNC
  | EL3HLT
  | EL3RST
  | ELIBACC
  | ELIBBAD
  | ELIBEXEC
  | ELIBMAX
  | ELIBSCN
  | ELNRNG
  | ELOOP
  | EMEDIUMTYPE
  | EMFILE
  | EMLINK
  | EMSGSIZE
  | EMULTIHOP
  | ENAMETOOLONG
  | ENAVAIL
  | ENETDOWN
  | ENETRESET
  | ENETUNREACH
  | ENFILE
  | ENOANO
  | ENOBUFS
  | ENOCSI
  | ENODATA
  | ENODEV
  | ENOENT
  | ENOEXEC
  | ENOKEY
  | ENOLCK
  | ENOLINK
  | ENOMEDIUM
  | ENOMEM
  | ENOMSG
  | ENONET
  | ENOPKG
  | ENOPROTOOPT
  | ENOSPC
  | ENOSR
  | ENOSTR
  | ENOSYS
  | ENOTBLK
  | ENOTCONN
  | ENOTDIR
  | ENOTEMPTY
  | ENOTNAM
  | ENOTRECOVERABLE
  | ENOTSOCK
  | ENOTTY
  | ENOTUNIQ
  | ENXIO
  | EOPNOTSUPP
  | EOVERFLOW
  | EOWNERDEAD
  | EPERM
  | EPFNOSUPPORT
  | EPIPE
  | EPROTO
  | EPROTONOSUPPORT
  | EPROTOTYPE
  | ERANGE
  | EREMCHG
  | EREMOTE
  | EREMOTEIO
  | ERESTART
  | EROFS
  | ESHUTDOWN
  | ESOCKTNOSUPPORT
  | ESPIPE
  | ESRCH
  | ESRMNT
  | ESTALE
  | ESTRPIPE
  | ETIME
  | ETIMEDOUT
  | ETOOMANYREFS
  | ETXTBSY
  | EUCLEAN
  | EUNATCH
  | EUSERS
  | EWOULDBLOCK
  | EXDEV
  | EXFULL
  | ERFKILL
  | UNKNOWN;

   local
      open Dynlib

      val dlh = Dynlib.dlopen {lib = "libcinfo.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }

      fun syminf s = Dynlib.dlsym dlh s;
      fun symp s = Dynlib.cptr (syminf s);

      val get_syserr_constants : unit -> int Vector.vector
        = Dynlib.app1
              (syminf "get_syserr_constants")

      val errno_vals = get_syserr_constants();

val errno_constants = #[
E2BIG,
EACCES,
EADDRINUSE,
EADDRNOTAVAIL,
EADV,
EAFNOSUPPORT,
EAGAIN,
EALREADY,
EBADE,
EBADF,
EBADFD,
EBADMSG,
EBADR,
EBADRQC,
EBADSLT,
EBFONT,
EBUSY,
ECANCELED,
ECHILD,
ECHRNG,
ECOMM,
ECONNABORTED,
ECONNREFUSED,
ECONNRESET,
EDEADLK,
EDEADLOCK,
EDESTADDRREQ,
EDOM,
EDOTDOT,
EDQUOT,
EEXIST,
EFAULT,
EFBIG,
EHOSTDOWN,
EHOSTUNREACH,
EIDRM,
EILSEQ,
EINPROGRESS,
EINTR,
EINVAL,
EIO,
EISCONN,
EISDIR,
EISNAM,
EKEYEXPIRED,
EKEYREJECTED,
EKEYREVOKED,
EL2HLT,
EL2NSYNC,
EL3HLT,
EL3RST,
ELIBACC,
ELIBBAD,
ELIBEXEC,
ELIBMAX,
ELIBSCN,
ELNRNG,
ELOOP,
EMEDIUMTYPE,
EMFILE,
EMLINK,
EMSGSIZE,
EMULTIHOP,
ENAMETOOLONG,
ENAVAIL,
ENETDOWN,
ENETRESET,
ENETUNREACH,
ENFILE,
ENOANO,
ENOBUFS,
ENOCSI,
ENODATA,
ENODEV,
ENOENT,
ENOEXEC,
ENOKEY,
ENOLCK,
ENOLINK,
ENOMEDIUM,
ENOMEM,
ENOMSG,
ENONET,
ENOPKG,
ENOPROTOOPT,
ENOSPC,
ENOSR,
ENOSTR,
ENOSYS,
ENOTBLK,
ENOTCONN,
ENOTDIR,
ENOTEMPTY,
ENOTNAM,
ENOTRECOVERABLE,
ENOTSOCK,
ENOTTY,
ENOTUNIQ,
ENXIO,
EOPNOTSUPP,
EOVERFLOW,
EOWNERDEAD,
EPERM,
EPFNOSUPPORT,
EPIPE,
EPROTO,
EPROTONOSUPPORT,
EPROTOTYPE,
ERANGE,
EREMCHG,
EREMOTE,
EREMOTEIO,
ERESTART,
EROFS,
ESHUTDOWN,
ESOCKTNOSUPPORT,
ESPIPE,
ESRCH,
ESRMNT,
ESTALE,
ESTRPIPE,
ETIME,
ETIMEDOUT,
ETOOMANYREFS,
ETXTBSY,
EUCLEAN,
EUNATCH,
EUSERS,
EWOULDBLOCK,
EXDEV,
EXFULL,
ERFKILL];

      val tbl' = Redblackmap.mkDict Int.compare

      val tbl = Vector.foldri (fn (i,c,tbl) => Redblackmap.insert (tbl,Vector.sub(errno_vals,i),c)) 
                  tbl' errno_constants 


            prim_val errno_    : unit -> int           = 1 "sml_errno";
	    prim_val errormsg_ : int -> string         = 1 "sml_errormsg";
   in
      fun fromInt errno =
        Redblackmap.find (tbl,errno) handle Redblackmap.NotFound => UNKNOWN

      fun toInt (syserr : syserr) =
         let val or = Obj.repr syserr
             val tag = Obj.obj_tag or
         in Vector.sub(errno_vals,tag)
         end

    exception SysErr of string * syserr

    fun toString syserr =
      errormsg_ (toInt syserr)

    val errno = fromInt o errno_

    fun raiseSysErr mlOp =
	 raise SysErr (mlOp, errno())
end
