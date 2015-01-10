#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>

#include "mosml/mlvalues.h"
#include "mosml/alloc.h"
#include "mosml/memory.h"
#include "mosml/fail.h"
#include "mosml/str.h"

value sigset_constants(value unit) {
  value res = alloc_tuple(2);

#ifndef _NSIG
#define _NSIG SIGRTMAX
#endif

  Field(res, 0) = Val_long(sizeof(sigset_t));
  Field(res, 1) = Val_long(_NSIG);
  return res;
}
