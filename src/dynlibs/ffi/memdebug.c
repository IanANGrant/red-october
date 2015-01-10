#include <string.h>
#include <stdlib.h>

/* Access to the camlrunm/Moscow ML runtime data representation: */

#include "mosml/mlvalues.h"
#include "mosml/fail.h"
#include "mosml/memory.h"
#include "mosml/alloc.h"
#include "mosml/callback.h"
#include "mosml/globals.h"
#include "mosml/interp.h"		/* For callback                            */
#include "mosml/str.h"		/* For string_length                       */

/* At most 50 Mb of FFI garbage to collect .... */
#define MAX_FFI_ALLOC 50000000

#define FFI_MEM_WITH_REALLOC

#include <stdio.h>
#include <stdalign.h>

long jit_ffi_debug = 0L;

#include "mem.h"

value memdebug_set(value d)
{
  if (Bool_val(d))
     jit_ffi_debug = 1L;
  else 
     jit_ffi_debug = 0L;

  return Val_unit;
}

value memdebug_dumpbytes (value cptr, value length)
{
  unsigned char *p = (unsigned char *) cptr;
  unsigned int addr = (unsigned int) cptr;
  int len = (int) Long_val(length);
  int i;

  if (len <= 0)
    return Val_unit;

  for (i = 0; i < len; i++) {
    if (i % 16 == 0)
      fprintf(stderr,"%s%8.8x : %2.2x", ((i == 0) ? "" : "\n"),addr+i,(unsigned int)(p[i]));
    else
      fprintf(stderr," %2.2x",(unsigned int)(p[i]));
  }
  fprintf(stderr,"\n");
  return Val_unit;
}

value memdebug_dumpwords (value cptr, value length)
{
  unsigned int *p = (unsigned int *) cptr;
  unsigned int addr = (unsigned int) cptr;
  int len = (int) Long_val(length);
  int i;

  if (len <= 0)
    return Val_unit;

  for (i = 0; i < len; i++) {
    if (i % 4 == 0)
      fprintf(stderr,"%s%8.8x : %8.8x", i == 0 ? "" : "\n",addr + i * sizeof(unsigned int) ,p[i]);
    else
      fprintf(stderr," %8.8x",p[i]);
  }
  fprintf(stderr,"\n");
  return Val_unit;
}

value memdebug_report_alloc (value msg)
{
  report_alloc(String_val(msg));
  return Val_unit;
}

typedef void (*final1_fun) (void *);

typedef void (*final2_fun) (void *,value);

#define Finalize1_val(x) (final1_fun) (Field(x, 3))
#define Finalize2_val(x) (final2_fun) (Field(x, 3))
#define Cptr_val(x) (void **)(&Field(x, 4))
#define Length_val(x) (Field(x, 5))
#define FieldsCount 6

#define Size (FieldsCount + 1)
#define HeapSpace (Size + sizeof(void *))

void memdebug_finalize1(value obj)
{
   final1_fun ffunp = Finalize1_val(obj);

   if (ffunp && !(Is_in_heap(*Cptr_val(obj)))) {
      if (jit_ffi_debug)
          fprintf(stderr,"memdebug_finalize1: calling %p to finalize %p.\n",
		  (void *) ffunp,*Cptr_val(obj));
      (*ffunp) (*Cptr_val(obj));
      Field(obj,3) = (value)NULL;
      *(Cptr_val(obj)) = NULL;
   }
   return;
}

void memdebug_finalize2(value obj)
{
   final2_fun ffunp = Finalize2_val(obj);

   if (ffunp && !(Is_in_heap(*Cptr_val(obj)))) {
      if (jit_ffi_debug)
          fprintf(stderr,"memdebug_finalize2: calling %p to finalize %p(length=%d).\n",
		  (void *) ffunp,*Cptr_val(obj),(size_t) Length_val(obj));
      (*ffunp) (*Cptr_val(obj), Length_val(obj));
      Field(obj,3) = (value)NULL;
      *(Cptr_val(obj)) = NULL;
   }
   return;
}
