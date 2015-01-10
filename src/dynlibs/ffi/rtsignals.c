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

value rtsignals_constants(value unit) {
  value res = alloc_tuple(6);

#ifndef CLOCK_MONOTONIC_RAW
#define CLOCK_MONOTONIC_RAW CLOCK_MONOTONIC
#endif

  Field(res, 0) = Val_long(CLOCK_REALTIME);
  Field(res, 1) = Val_long(CLOCK_MONOTONIC);
  Field(res, 2) = Val_long(CLOCK_MONOTONIC_RAW);
  Field(res, 3) = Val_long(CLOCK_PROCESS_CPUTIME_ID);
  Field(res, 4) = Val_long(CLOCK_THREAD_CPUTIME_ID);
  Field(res, 5) = Val_long(TIMER_ABSTIME);
  return res;
}

value rtsignals_signals(value unit) {
  value res = alloc_tuple(2);

  Field(res, 0) = Val_long(SIGRTMIN);
  Field(res, 1) = Val_long(SIGRTMAX);
  return res;
}
