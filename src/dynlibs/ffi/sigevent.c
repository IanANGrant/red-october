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

value sigevent_constants(value unit) {
  value res = alloc_tuple(10);
  struct sigevent sev;

#define sev_offset(field) ((char *)&sev.field - (char *)&sev)

  Field(res, 0) = Val_long(sizeof(struct sigevent));
  Field(res, 1) = Val_long(sev_offset(sigev_notify));
  Field(res, 2) = Val_long(sev_offset(sigev_signo));
  Field(res, 3) = Val_long(sev_offset(sigev_value));
  Field(res, 4) = Val_long(sev_offset(sigev_notify_function));
  Field(res, 5) = Val_long(sev_offset(sigev_notify_attributes));
  /* Field(res, 6) = Val_long(sev_offset(sigev_notify_thread_id)); */
  Field(res, 6) = Val_long(SIGEV_NONE);
  Field(res, 7) = Val_long(SIGEV_SIGNAL);
  Field(res, 8) = Val_long(SIGEV_THREAD);
  Field(res, 9) = Val_long(SIGEV_THREAD_ID);
  return res;
}

