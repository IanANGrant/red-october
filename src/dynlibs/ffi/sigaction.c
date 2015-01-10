#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <time.h>

#include "mosml/mlvalues.h"
#include "mosml/alloc.h"
#include "mosml/memory.h"
#include "mosml/fail.h"
#include "mosml/str.h"

value sigaction_constants(value unit) {
  value res = alloc_tuple(10);
  Field(res,0) = Val_long(SA_NOCLDSTOP);
  Field(res,1) = Val_long(SA_NOCLDWAIT);
  Field(res,2) = Val_long(SA_SIGINFO);
  Field(res,3) = Val_long(SA_ONSTACK);
  Field(res,4) = Val_long(SA_RESTART);
  Field(res,5) = Val_long(SA_INTERRUPT);
  Field(res,6) = Val_long(SA_NODEFER);
  Field(res,7) = Val_long(SA_RESETHAND);
  Field(res,8) = Val_long(SIG_DFL);
  Field(res,9) = Val_long(SIG_IGN);
  return res;
}
