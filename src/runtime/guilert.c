#include "config.h"
#include "fail.h"

#ifdef HAVE_GUILE

#include <libguile.h>

extern int caml_main (int argc, char **argv);

struct argstr {
  int argc;
  char **argv;
};

static SCM main_call (struct argstr *data)
{
  fprintf(stderr,"debug: main_call: calling caml_main\n");
  (void) caml_main(data->argc, data->argv);
  fprintf(stderr,"debug: main_call: caml_main reurned\n");
  return SCM_UNSPECIFIED;
}

static SCM main_handler(void *data, SCM key, SCM args)
{
  fprintf(stderr,"debug: main_handler called\n");
  return SCM_UNSPECIFIED;
}

static SCM main_pre_unwind_handler(void *data, SCM key, SCM args)
{
  fprintf(stderr,"debug: main_pre_unwind_handler called\n");
  failwith("Uncaught scheme exception");
  fprintf(stderr,"debug: main_pre_unwind_handler: failwith returned!\n");
  return SCM_UNSPECIFIED; // Never reached
}

static void main_trampoline(void *data, int argc, char *argv[])
{
  static struct argstr bdata;

  bdata.argc = argc;
  bdata.argv = argv;

  // (void) main_call(&bdata);
  fprintf(stderr,"debug: main_trampoline: calling scm_c_catch"
                                 "(main_call,main_handler,main_pre_unwind_handler)\n");
  // (void) scm_c_with_throw_handler (SCM_BOOL_T, (scm_t_catch_body) &main_call, &bdata,
  //                                (scm_t_catch_handler) &main_pre_unwind_handler, NULL,0);
  (void) scm_c_catch (SCM_BOOL_T, (scm_t_catch_body) &main_call, &bdata,
		                  (scm_t_catch_handler) &main_handler, NULL,
                                  (scm_t_catch_handler) &main_pre_unwind_handler, NULL);

  fprintf(stderr,"debug: main_trampoline: scm_c_catch returned!\n");
}

int main (int argc, char **argv)
{
   scm_boot_guile (argc, argv, main_trampoline, 0);
   return 0; /* never reached. */
}
#endif
