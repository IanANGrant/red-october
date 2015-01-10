#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include "mosml/mlvalues.h"
#include "mosml/alloc.h"
#include "mosml/memory.h"
#include "mosml/fail.h"
#include "mosml/str.h"

/* Decomposition of sock_ values: */
#define Sock_val(x) (Field(x,0))

/* Warning: allocates in the heap, may cause a GC */
/* ML return type: sock_ */
value fdset_fddesc(value fd) {
  value result = alloc(1, SOMEtag);
  Sock_val(result) = fd;
  return result;
}

/* ML return type: int */
value fdset_descfd(value desc) {
   value result = Sock_val(desc);
   return result;
}

value fdset_openmax(value unit) {
  value res;
  res = Val_long(sysconf(_SC_OPEN_MAX));
  return res;
}

value fdset_fdsetsize(value unit) {
  value res;
  res = Val_long(FD_SETSIZE);
  return res;
}

value fdset_size(value unit) {
  return Val_long(sizeof(fd_set));
}

value fdset_set(value fd,fd_set *fds) {
  FD_SET(Long_val(fd),fds);
  /* fprintf(stderr,"fdset_set: fd = %d.\n",Long_val(fd)); */
  return Val_unit;
}

value fdset_clr(value fd,fd_set *fds) {
  FD_CLR(Long_val(fd),fds);
  /* fprintf(stderr,"fdset_clr: fd = %d.\n",Long_val(fd)); */
  return Val_unit;
}

value fdset_zero(fd_set *fds) {
  FD_ZERO(fds);
  return Val_unit;
}

value fdset_isset(value fd, fd_set *fds) {
  /* fprintf(stderr,"fdset_isset: fd = %d.\n",Long_val(fd)); */
  return (FD_ISSET(Long_val(fd),fds) == 0 ? Val_false : Val_true);
}
