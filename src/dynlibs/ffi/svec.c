#include <stdio.h>

#include "mffi.h"
#include "mem.h"

extern long jit_ffi_debug;

// Finalised objects representing buffers as ML byte-vectors on the
// static heap, (i.e. not subject to garbage collection).

/* Field 0 is the finalized tag. */
#define Svec_val(x) (void **)(&Field(x, 1))
#define SvecLength_val(x) (mlsize_t)(Field(x, 2))
#define SvecFieldsCount 2

#define SvecSize (SvecFieldsCount + 1)
#define SvecHeapSpace (SvecSize + sizeof(void *))

void svec_finalize(value obj)
{
  mosml_ffi_free(*Svec_val(obj));
  return;
}

value svec_clear(value obj)			
{
  svec_finalize(obj);
  Tag_val(obj) = Abstract_tag;
  return Val_unit;
}

value svec_make(value lengthv)
{ 
  value sv;

  sv = alloc_final(SvecSize, &svec_finalize, SvecHeapSpace, MAX_FFI_ALLOC);
  initialize((value *) Svec_val(sv),(value) mosml_ffi_alloc((mlsize_t) Long_val(lengthv)));
  initialize(&Field(sv,2),Long_val(lengthv));

  return sv;
}

value svec_getlength(value sv)
{
  value res = Val_long(SvecLength_val(sv));
  return res;
}

value svec_getpointervalue(value sv)
{
  value res;

  Push_roots(r, 1);
  r[0] = sv;
  res = alloc_string(sizeof(void *));
  bcopy(Svec_val(r[0]), String_val(res), sizeof(void *));

  Pop_roots();
  return res;
}

value svec_getcptrvalue(value sv)
{
  value res;
  
  Push_roots(r, 1);
  r[0] = sv;
  res = alloc_string(sizeof(void *));
  bcopy((char *)&r, String_val(res), sizeof(void *));

  if (jit_ffi_debug)
     fprintf(stderr,"svec_getcptrvalue returning 0x%8.8x [0x%8.8x].\n",
          *(unsigned int *)(&r), *(unsigned int *) res);

  Pop_roots();
  return res;
}

value svec_getcptrword (value cptr)
{
  return Val_long(*(unsigned int *)&(cptr));
}

value svec_getcptr (value v)
{
  return *((value *) v);
}

value svec_getcptrwordv (value cptr)
{
  value res;

  Push_roots(r, 1);
  r[0] = cptr;
  res = alloc_tuple(1);
  modify(&Field(res, 0), Val_long(*(unsigned int *)r));

  if (jit_ffi_debug)
     fprintf(stderr,"svec_getcptrwordv returning 0x%8.8x [0x%8.8x].\n",
	     *(unsigned int *)r, (unsigned int) Long_val(Field(res, 0)));
  Pop_roots();

  return res;
}

value svec_setcptrvalue(value vec)
{
  value res;
  
  Push_roots(r, 1);
  r[0] = vec;
  res = alloc_string(sizeof(void *));
  bcopy(String_val(r[0]), Op_val(res), sizeof(void *));

  if (jit_ffi_debug)
     fprintf(stderr,"svec_setcptrvalue returning 0x%8.8x [0x%8.8x].\n",
          *(unsigned int *)(String_val(r[0])), *(unsigned int *) res);

  Pop_roots();
  return res;
}

value svec_getvecword (value vec)
{
  value res;

  res = Val_long(*(unsigned int*) String_val(vec));

  if (jit_ffi_debug)
    fprintf(stderr,"svec_getvecword returning 0x%8.8x [0x%8.8x].\n",
            (unsigned int) Long_val(res), * ((unsigned int*) (String_val(vec))));
  return res;
}

value svec_setvecword (value word)
{
  value res;
  unsigned int src = Long_val(word);

  res = alloc_string(sizeof(unsigned int));
  bcopy((char *)&src, String_val(res), sizeof(unsigned int));

  if (jit_ffi_debug)
    fprintf(stderr,"svec_setvecword returning 0x%8.8x [0x%8.8x].\n",
                    *(unsigned int*) res, (unsigned int) src);

  return res;
}

value *svec_setcptrword(value word)
{
  value res = Long_val(word);

  if (jit_ffi_debug)
     fprintf(stderr,"svec_setcptrword returning 0x%8.8x.\n", (unsigned int) res);

   return (value *) res;
}

value *svec_getbuffer (value sv)
{
  value *res = (value *) (*Svec_val(sv));

  if (jit_ffi_debug)
     fprintf(stderr,"svec_getbuffer returning %p.\n", (void *) res);

  return res;
}

value svec_getvalue(value sv, value offset, value length)
{
  value res;
  mlsize_t offs;
  mlsize_t len, avail;
  char *src;

  len = Long_val(length);
  offs = Long_val (offset);
  avail = SvecLength_val(sv) - offs;

  if (avail < len)
    len = avail;

  if (len < 1)
    return alloc_string(0);

  {
      Push_roots(r, 1);
      r[0] = sv;
      res = alloc_string(len);
      src = ((char *) (*Svec_val(r[0])))+offs;

      bcopy(src, String_val(res), len);

      if (jit_ffi_debug)
         fprintf(stderr,"svec_getvalue: copied %ld bytes from %p to %p.\n", 
                            len, (void *) src, String_val(res));

      Pop_roots();
  }
  return res;
}

value svec_setvalue(value svec, value offset, value str)
{
  mlsize_t len, offs;
  mlsize_t max_headroom;
  char *dst, *src;

  offs = Long_val(offset);
  len = string_length(str);
  src = String_val(str);
  dst = (((char *)*(Svec_val(svec)))+offs);

  if ((max_headroom = (SvecLength_val(svec) - offs)) <= 0)
    return Val_long(0);

  if (len > max_headroom)
    len = max_headroom;

  if (jit_ffi_debug)
     fprintf(stderr,"svec_setvalue: copying %ld [%ld] bytes from %p [=+%ld] to %p.\n", 
	     len, max_headroom, (void *) src, offs, (void *) dst);

  bcopy(src, dst, len);

  return Val_long(len);
}
