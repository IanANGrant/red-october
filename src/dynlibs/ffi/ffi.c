#include <stdio.h>
#include <stdalign.h>

#include "mffi.h"
#include "mem.h"

#include "types.h"

extern long jit_ffi_debug;

/* All we really need are the offsets of the fields in the structure,
   and the constant values because at this stage, the only thing that
   is likely to work is the default ABI. Once we are bootstrapped, we
   will have the details of the binary API such as length, alignment,
   byte-order etc. intentionally specified. For the bootstrap however,
   we need only know what these things are _de facto._ */

static const intstrpair_t status_values[] = {
  {FFI_OK,           "OK"},
  {FFI_BAD_TYPEDEF, "BAD_TYPEDEF"},
  {FFI_BAD_ABI,     "BAD_ABI"} 
};

static int statusvalueslens = (int) (sizeof(status_values)) / (sizeof(struct intstrpair_));

static const intstrpair_t type_values[] = {
 {FFI_TYPE_VOID, "VOID"}, 
 {FFI_TYPE_INT, "INT"},
 {FFI_TYPE_FLOAT, "FLOAT"},
 {FFI_TYPE_DOUBLE, "DOUBLE"},
 {FFI_TYPE_LONGDOUBLE, "LONGDOUBLE"},
 {FFI_TYPE_UINT8, "UINT8"},
 {FFI_TYPE_SINT8, "SINT8"},
 {FFI_TYPE_UINT16, "UINT16"},
 {FFI_TYPE_SINT16, "SINT16"},
 {FFI_TYPE_UINT32, "UINT32"},
 {FFI_TYPE_SINT32, "SINT32"},
 {FFI_TYPE_UINT64, "UINT64"},
 {FFI_TYPE_SINT64, "SINT64"},
 {FFI_TYPE_STRUCT, "STRUCT"},
 {FFI_TYPE_POINTER, "POINTER"}};

static int typevalueslens = (int) (sizeof(type_values)) / (sizeof(struct intstrpair_));

static const intstrtriple_t type_sizes[] = {
  {0, 0,                                                "VOID"},
  {(int) alignof(signed int), (int) sizeof(signed int), "INT"},
  {(int) alignof(jit_float32_t), (int) sizeof(jit_float32_t), "FLOAT"},
  {(int) alignof(jit_float64_t), (int) sizeof(jit_float64_t), "DOUBLE"},
  {(int) alignof(long double), (int) sizeof(long double), "LONGDOUBLE"},
  {(int) alignof(jit_uint8_t), (int) sizeof(jit_uint8_t), "UINT8"},
  {(int) alignof(jit_int8_t), (int) sizeof(jit_int8_t), "SINT8"},
  {(int) alignof(jit_uint16_t), (int) sizeof(jit_uint16_t), "UINT16"},
  {(int) alignof(jit_int16_t), (int) sizeof(jit_int16_t), "SINT16"},
  {(int) alignof(jit_uint32_t), (int) sizeof(jit_uint32_t), "UINT32"},
  {(int) alignof(jit_int32_t), (int) sizeof(jit_int32_t), "SINT32"},
  {(int) alignof(jit_uint64_t), (int) sizeof(jit_uint64_t), "UINT64"},
  {(int) alignof(jit_int64_t), (int) sizeof(jit_int64_t), "SINT64"},
  {0, 0,                                                 "STRUCT"},
  {(int) alignof(jit_pointer_t), (int) sizeof(jit_pointer_t), "POINTER"}};

static int typesizeslens = (int) (sizeof(type_sizes)) / (sizeof(struct intstrtriple_));

static const ptrstrpair_t type_struct_pointers[] = {
 {(const void *) &ffi_type_void, "void"}, 
 {(const void *) &ffi_type_uchar, "uchar"},
 {(const void *) &ffi_type_schar, "schar"},
 {(const void *) &ffi_type_uint, "uint"},
 {(const void *) &ffi_type_sint, "sint"},
 {(const void *) &ffi_type_ushort, "ushort"},
 {(const void *) &ffi_type_sshort, "sshort"},
 {(const void *) &ffi_type_ulong, "ulong"},
 {(const void *) &ffi_type_slong, "slong"},
 {(const void *) &ffi_type_float, "float"},
 {(const void *) &ffi_type_double, "double"},
 {(const void *) &ffi_type_longdouble, "longdouble"},
 {(const void *) &ffi_type_pointer, "pointer"},
 {(const void *) &ffi_type_uint8, "uint8"},
 {(const void *) &ffi_type_sint8, "sint8"},
 {(const void *) &ffi_type_uint16, "uint16"},
 {(const void *) &ffi_type_sint16, "sint16"},
 {(const void *) &ffi_type_uint32, "uint32"},
 {(const void *) &ffi_type_sint32, "sint32"},
 {(const void *) &ffi_type_uint64, "uint64"},
 {(const void *) &ffi_type_sint64, "sint64"}};

static int typestructpointerslens = (int) (sizeof(type_struct_pointers)) / (sizeof(struct ptrstrpair_));

static const ffi_type ts;

#define elt_offset(a,b) ((mlsize_t) (((char *) &a.b - (char *) &a)))
#define elt_length(a) ((mlsize_t) (sizeof a))

static const  intstrtriple_t type_struct[] = {
  {elt_offset(ts,size),      elt_length(ts.size),      "size"},
  {elt_offset(ts,alignment), elt_length(ts.alignment), "alignment"},
  {elt_offset(ts,type),      elt_length(ts.type),      "type"},
  {elt_offset(ts,elements),  elt_length(ts.elements),  "elements"}};

static int typestructlens = (int) (sizeof(type_struct)) / (sizeof(struct intstrtriple_));

value ffi_default_abi(value unit)
{
  return Val_long((long) FFI_DEFAULT_ABI);
}

value ffi_cif_length(value unit)
{
  return Val_long((mlsize_t)(sizeof(ffi_cif)));
}

value ffi_type_length(value unit)
{
  return Val_long((mlsize_t)(sizeof(ffi_type)));
}

value ffi_closure_length(value unit)
{
  return Val_long((mlsize_t)(sizeof(ffi_closure)));
}

value ffi_get_statusvalueslength(value unit)
{
  return Val_long(statusvalueslens);
}

value ffi_get_statusvaluesentry(value num)
{   int entry = Long_val(num);

    if (entry >= 0 && entry < statusvalueslens)
      return copy_int_str_pair((intstrpair_t *)&status_values[entry]);
    return copy_int_str_pair((intstrpair_t *)&status_values[0]);
}

value ffi_get_typevaluesentry(value num)
{   int entry = Long_val(num);

    if (entry >= 0 && entry < typevalueslens)
      return copy_int_str_pair((intstrpair_t *)&type_values[entry]);
    return copy_int_str_pair((intstrpair_t *)&type_values[0]);
}

value ffi_get_typesizesentry(value num)
{   int entry = Long_val(num);

    if (entry >= 0 && entry < typesizeslens)
      return copy_int_str_triple((intstrtriple_t *)&type_sizes[entry]);
    return copy_int_str_triple((intstrtriple_t *)&type_sizes[0]);
}

value ffi_get_typestructentry(value num)
{   int entry = Long_val(num);

    if (entry >= 0 && entry < typestructlens)
      return copy_int_str_triple((intstrtriple_t *)&type_struct[entry]);
    return copy_int_str_triple((intstrtriple_t *)&type_struct[0]);
}

value ffi_get_typestructpointersentry(value num)
{   int entry = Long_val(num);

    if (entry >= 0 && entry < typestructpointerslens)
      return copy_ptr_str_pair((ptrstrpair_t *)&type_struct_pointers[entry]);
    return copy_ptr_str_pair((ptrstrpair_t *)&type_struct_pointers[0]);
}

value ffi_get_typevalueslength(value unit)
{
  return Val_long(typevalueslens);
}

value ffi_get_typestructlength(value unit)
{
  return Val_long(typestructlens);
}

value ffi_get_typesizeslength(value unit)
{
  return Val_long(typesizeslens);
}

value ffi_get_typestructpointerslength(value unit)
{
  return Val_long(typestructpointerslens);
}

int ffi_dryrun = 0;

value ffi_setdryrun (value f)
{
  if (Bool_val(f)) 
    ffi_dryrun = 1;
  else
    ffi_dryrun = 0;

  return Val_unit;
}

value ffi_getdryrun (value unit)
{
  value res = Val_false;

  if (ffi_dryrun)
    res = Val_true;
  
  return res;
}

value ffi_dumpbytes (value cptr, value length)
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

value ffi_dumpwords (value cptr, value length)
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

value ffi_prep_cif_ (ffi_cif *cifptr, value vabi, value vnargs, ffi_type *rtype, ffi_type **argp)
{
  value res;
  unsigned int resi;
  unsigned int nargs, abi, i;

  abi = (unsigned int) Long_val(vabi);
  nargs = (unsigned int) Long_val(vnargs);

  if (jit_ffi_debug) {
     fprintf(stderr, "ffi_prep_cif_: cifptr = %p, abi = %d, nargs = %d, rtype = %p, args = %p\n",
	     (void *) cifptr, abi, nargs, (void *) rtype, (void *) argp);
     for (i = 0; i < nargs; i++) {
       ffi_type *tp = *((argp)+i);
       fprintf(stderr, "ffi_prep_cif_: args[%d] = %p->type = %d\n", i, (void *) (tp),
                                                             (unsigned int) tp->type);
     }
  }

  if (!ffi_dryrun)
     resi = ffi_prep_cif((ffi_cif *) cifptr, abi, nargs, (ffi_type *) rtype, argp);
  else
     resi = 0;

  res = Val_long (resi);

  if (jit_ffi_debug)
    fprintf(stderr, "ffi_prep_cif_: res = %8.8lx\n", (unsigned long) Long_val(res));

  return res;
}

value ffi_call_ (ffi_cif *cifptr, void (*fptr) (void), value rvptr, value argsptr)
{
   int i;
   int rv;
   ffi_cif *cp = cifptr;
   ffi_type **argtp = cifptr->arg_types;

   if (jit_ffi_debug) {
      fprintf(stderr, "ffi_call_: cifptr = %p, fptr = %p, rvptr = %p, argsptr = %p\n",
                           (void *) cp, (void *) fptr, (void *) rvptr, (void *) argsptr);
      for (i = 0; i < cifptr->nargs; i++) {
        ffi_type *tp = *((argtp)+i);
        unsigned int tn = tp->type;
	fprintf(stderr, "ffi_call_: args[%d] = %p [->%p]\n", i, *(((void **) argsptr) + i),
                                 (tn == FFI_TYPE_POINTER) ? **(((void ***) argsptr) + i) : NULL);
      }
   }
   if (!ffi_dryrun) 
      ffi_call(cifptr, fptr, (int *)rvptr, (void **) &argsptr);

   if (jit_ffi_debug) {
      fprintf(stderr, "ffi_call returned OK.\n");
   }

   return Val_unit;
}

value ffi_call_n_ (ffi_cif *cifptr, void (*fptr) (void), value rvptr, value argsptr)
{
   int i;
   int rv;

   if (jit_ffi_debug) {
      fprintf(stderr, "ffi_call_n_: cifptr = %p, fptr = %p, rvptr = %p, argsptr = %p\n",
                           (void *) cifptr, (void *) fptr, (void *) rvptr, (void *) argsptr);
      for (i = 0; i < cifptr->nargs; i++) {
	fprintf(stderr, "ffi_call_n_: args[%d] = %p\n", i, *(((void **) argsptr) + i));
      }
   }
   if (!ffi_dryrun) 
      ffi_call(cifptr, fptr, (int *)rvptr, (void **) argsptr);

   if (jit_ffi_debug) {
      fprintf(stderr, "ffi_call_n_ returned OK.\n");
   }

   return Val_unit;
}

value ffi_report_alloc (value msg)
{
  report_alloc(String_val(msg));
  return Val_unit;
}

