#include "config.h"
#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"
#include "signals.h"
#include "globals.h"

#ifdef HAVE_GUILE

#include <libguile.h>

extern int caml_main (int argc, char **argv);

struct argstr {
  int argc;
  char **argv;
};

static SCM main_call (struct argstr *data)
{
  (void) caml_main(data->argc, data->argv);
  return SCM_UNSPECIFIED;
}

static SCM main_handler(void *data, SCM key, SCM args)
{
  return SCM_UNSPECIFIED;
}

void scm_sml_raise1(int exnindex, SCM arg) {
  value exn;
  exn = alloc_tuple(2);
  modify(&Field(exn, 0), Field(global_data, exnindex));
  modify(&Field(exn, 1), (value) arg);
  mlraise(exn);
}

static SCM main_pre_unwind_handler(void *data, SCM key, SCM args)
{
  scm_sml_raise1(SYS__EXN_LANGUAGE, scm_cons (key,args));
  return SCM_UNSPECIFIED; // Never reached
}

static void main_trampoline(void *data, int argc, char *argv[])
{
  static struct argstr bdata;

  bdata.argc = argc;
  bdata.argv = argv;

  (void) scm_c_catch (SCM_BOOL_T, (scm_t_catch_body) &main_call, &bdata,
		                  (scm_t_catch_handler) &main_handler, NULL,
		      (scm_t_catch_handler) &main_pre_unwind_handler, NULL, 1);
}

int main (int argc, char **argv)
{
   scm_boot_guile (argc, argv, main_trampoline, 0);
   return 0; /* never reached. */
}
#endif
