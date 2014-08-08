ACLOCAL_AMFLAGS = -I m4

SUBDIRS = runtime mosmlyac mosmllib compiler lex launch

if !NOTHING

if FFI
   SUBDIRS += dynlibs/ffi
endif

if POSTGRESQL
  SUBDIRS += dynlibs/mpq
endif

if REGEX
  SUBDIRS += dynlibs/mregex
endif

if CRYPT
  SUBDIRS += dynlibs/crypt
endif

if UNIX
  SUBDIRS += dynlibs/munix
endif

if INTINF
  SUBDIRS += dynlibs/intinf
endif

if SOCKETS
  SUBDIRS += dynlibs/msocket
endif

endif !NOTHING
