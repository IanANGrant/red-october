#include <stdio.h>

#include "major_gc.h"
#include "mffi.h"
#include "mem.h"

extern long jit_ffi_debug;

// Finalised objects representing buffers as ML byte-vectors on the
// static heap, (i.e. not subject to garbage collection).

typedef void (*svec_final_fun) (void *);

typedef void (*svec_final2_fun) (void *,value);

/* Field 0 is the finalized tag. */
#define Svec_val(x) (void **)(&Field(x, 1))
#define SvecLength_val(x) (Long_val(Field(x, 2)))
#define SvecFinalize_val(x) (svec_final_fun) (Field(x, 3))
#define SvecFinalize2_val(x) (svec_final2_fun) (Field(x, 3))
#define SvecLength2_val(x) (Long_val(Field(x, 4)))
#define SvecFieldsCount 4

#define SvecSize (SvecFieldsCount + 1)
#define SvecHeapSpace (SvecSize + sizeof(void *))

void svec_finalize(value obj)
{
   svec_final_fun ffunp = SvecFinalize_val(obj);

   /* See the mosml_ffi_* functions in mem.c for the in-memory format
      of the blocks allocated. They are meant to look exactly like
      objects of type String.string or Word8Vector.vector. This fools
      the interpreter and so aliasing one of these things with a val
      binding will create an on-heap copy of the whole buffer. So we
      only trigger the finalise function when the object is
      off-heap. */

   if (ffunp && !(Is_in_heap(*Svec_val(obj)))) {
      if (jit_ffi_debug)
          fprintf(stderr,"svec_finalise: calling %p to finalise %p.\n",
                          (void *) ffunp,*Svec_val(obj));
      (*ffunp) (*Svec_val(obj));
      Field(obj,3) = (value)NULL;
      *(Svec_val(obj)) = NULL;
   }
   return;
}

void svec_finalize2(value obj)
{
   svec_final2_fun ffunp = SvecFinalize2_val(obj);

   /* See the comment on svec_finalize, above. */

   if (ffunp && !(Is_in_heap(*Svec_val(obj)))) {
      if (jit_ffi_debug)
          fprintf(stderr,"svec_finalize2: calling %p to finalize %p(length=%d).\n",
		  (void *) ffunp,*Svec_val(obj),(size_t) Field(obj, 4));
      (*ffunp) (*Svec_val(obj), Field(obj, 4));
      Field(obj,3) = (value)NULL;
      *(Svec_val(obj)) = NULL;
   }
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
  initialize((value *) Svec_val(sv), (value) mosml_ffi_alloc((size_t) Long_val(lengthv)));
  initialize(&Field(sv,2),lengthv);
  initialize(&Field(sv,3),(value) &mosml_ffi_free);

  return sv;
}

/* Here the allocation and finalize functions are separate: the buffer
   could just as easily have been allocated by some library
   function. So we take pointers to the buffer and the finalize
   function. This is not necessarily the same type as the static
   vector created by svec_make, because the buffer is not necessarily
   something that looks like a Word8Vector: it could be a pointer to
   any sort of structure or union returned by an arbitrary
   function. But in the event that we _know_ the buffer pointer is a
   pointer to a Word8Vector-like block (because it was allocated by
   mosml_ffi_alloc, for example!) then we can recast this object to an
   ordinary static vector. */

value svec_wrap_cptr(value cptr, value finalize, value lengthv)
{ 
  value sv;

  sv = alloc_final(SvecSize, &svec_finalize , SvecHeapSpace, MAX_FFI_ALLOC);
  initialize((value *) Svec_val(sv),cptr);
  initialize(&Field(sv,2),lengthv);
  initialize(&Field(sv,3), finalize);
  return sv;
}

/* Similar, but calls the finalize fn with the pointer and the length,
   as required by, e.g. munmap */
value svec_wrap_cptr2(value cptr, value finalize, value lengthv)
{ 
  value sv, buffv;
  size_t len;

  len = Long_val(lengthv) - sizeof(header_t) - sizeof(value);

  if (jit_ffi_debug)
      fprintf(stderr,"svec_wrap_cptr2: calling alloc_final: finalize=%p  buffer=%p(length=%ld[%d]).\n",
	      (void *) finalize,(void *)cptr,Long_val(lengthv),len);
  sv = alloc_final(SvecSize + 1, &svec_finalize2 , SvecHeapSpace, MAX_FFI_ALLOC);
  if (jit_ffi_debug)
      fprintf(stderr,"svec_wrap_cptr2: calling mosml_ffi_alloc_wrap: buffer=%p(length=%d).\n",
	      (void *)cptr,len);
  buffv = (value) mosml_ffi_alloc_wrap((void *)cptr,len);
  if (jit_ffi_debug)
      fprintf(stderr,"svec_wrap_cptr2: initializing: finalize=%p  buffv=%p(length=%d[%ld]).\n",
	      (void *) finalize,(void *)buffv,len,string_length(buffv));
  initialize((value *) Svec_val(sv), buffv);
  initialize(&Field(sv,2), Val_long(len));
  initialize(&Field(sv,3), finalize);
  initialize(&Field(sv,4), Long_val(lengthv));
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
  return (value) (*(unsigned int *)res);
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
  offs = Long_val(offset);
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
    return (Val_long(0));

  if (len > max_headroom)
    len = max_headroom;

  if (jit_ffi_debug)
     fprintf(stderr,"svec_setvalue: copying %ld [%ld] bytes from %p [=+%ld] to %p.\n", 
	     len, max_headroom, (void *) src, offs, (void *) dst);

  bcopy(src, dst, len);

  return Val_long(len);
}
