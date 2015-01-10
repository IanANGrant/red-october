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

#ifndef BUS_MCEERR_AR
#define BUS_MCEERR_AR 0
#endif
#ifndef BUS_MCEERR_AO
#define BUS_MCEERR_AO 0
#endif
#ifndef TRAP_BRANCH
#define  TRAP_BRANCH 0
#endif
#ifndef TRAP_HWBKPT
#define TRAP_HWBKPT 0
#endif

value si_code_values(value unit) {
   value res = alloc_tuple(47);
   Field(res, 0) = Val_long(SI_USER);
   Field(res, 1) = Val_long(SI_KERNEL);
   Field(res, 2) = Val_long(SI_QUEUE);
   Field(res, 3) = Val_long(SI_TIMER);
   Field(res, 4) = Val_long(SI_MESGQ);
   Field(res, 5) = Val_long(SI_ASYNCIO);
   Field(res, 6) = Val_long(SI_SIGIO);
   Field(res, 7) = Val_long(SI_TKILL);
   Field(res, 8) = Val_long(ILL_ILLOPC);
   Field(res, 9) = Val_long(ILL_ILLOPN);
   Field(res, 10) = Val_long(ILL_ILLADR);
   Field(res, 11) = Val_long(ILL_ILLTRP);
   Field(res, 12) = Val_long(ILL_PRVOPC);
   Field(res, 13) = Val_long(ILL_PRVREG);
   Field(res, 14) = Val_long(ILL_COPROC);
   Field(res, 15) = Val_long(ILL_BADSTK);
   Field(res, 16) = Val_long(FPE_INTDIV);
   Field(res, 17) = Val_long(FPE_INTOVF);
   Field(res, 18) = Val_long(FPE_FLTDIV);
   Field(res, 19) = Val_long(FPE_FLTOVF);
   Field(res, 20) = Val_long(FPE_FLTUND);
   Field(res, 21) = Val_long(FPE_FLTRES);
   Field(res, 22) = Val_long(FPE_FLTINV);
   Field(res, 23) = Val_long(FPE_FLTSUB);
   Field(res, 24) = Val_long(SEGV_MAPERR);
   Field(res, 25) = Val_long(SEGV_ACCERR);
   Field(res, 26) = Val_long(BUS_ADRALN);
   Field(res, 27) = Val_long(BUS_ADRERR);
   Field(res, 28) = Val_long(BUS_OBJERR);
   Field(res, 29) = Val_long(BUS_MCEERR_AR);
   Field(res, 30) = Val_long(BUS_MCEERR_AO);
   Field(res, 31) = Val_long(TRAP_BRKPT);
   Field(res, 32) = Val_long(TRAP_TRACE);
   Field(res, 33) = Val_long(TRAP_BRANCH);
   Field(res, 34) = Val_long(TRAP_HWBKPT);
   Field(res, 35) = Val_long(CLD_EXITED);
   Field(res, 36) = Val_long(CLD_KILLED);
   Field(res, 37) = Val_long(CLD_DUMPED);
   Field(res, 38) = Val_long(CLD_TRAPPED);
   Field(res, 39) = Val_long(CLD_STOPPED);
   Field(res, 40) = Val_long(CLD_CONTINUED);
   Field(res, 41) = Val_long(POLL_IN);
   Field(res, 42) = Val_long(POLL_OUT);
   Field(res, 43) = Val_long(POLL_MSG);
   Field(res, 44) = Val_long(POLL_ERR);
   Field(res, 45) = Val_long(POLL_PRI);
   Field(res, 46) = Val_long(POLL_HUP);
  return res;
}

value siginfo_constants(value unit) {
  value res = alloc_tuple(17);
  siginfo_t si;

#define si_offset(field) ((char *)&si.field - (char *)&si)

  Field(res, 0) = Val_long(sizeof(siginfo_t));
  Field(res, 1) = Val_long(si_offset(si_signo));
  Field(res, 2) = Val_long(si_offset(si_errno));
  Field(res, 3) = Val_long(si_offset(si_code));
  Field(res, 4) = Val_long(si_offset(si_pid));
  Field(res, 5) = Val_long(si_offset(si_uid));
  Field(res, 6) = Val_long(si_offset(si_status));
  Field(res, 7) = Val_long(si_offset(si_utime));
  Field(res, 8) = Val_long(si_offset(si_stime));
  Field(res, 9) = Val_long(si_offset(si_value));
  Field(res, 10) = Val_long(si_offset(si_int));
  Field(res, 11) = Val_long(si_offset(si_ptr));
  Field(res, 12) = Val_long(si_offset(si_overrun));
  Field(res, 13) = Val_long(si_offset(si_timerid));
  Field(res, 14) = Val_long(si_offset(si_addr));
  Field(res, 15) = Val_long(si_offset(si_band));
  Field(res, 16) = Val_long(si_offset(si_fd));
  return res;
}
