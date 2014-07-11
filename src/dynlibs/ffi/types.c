#include "mffi.h"

#define __NO_DECL
#include "types.h"
#undef __NO_DECL

value copy_int_str_triple(const intstrtriple_t *p)
{
  value tup = alloc_tuple(3);
  modify(&Field(tup, 0), Val_long(p->offset));
  modify(&Field(tup, 1), Val_long(p->length));
  modify(&Field(tup, 2), copy_string((char *) p->name));
  return tup;
}

value copy_int_str_pair(const intstrpair_t *p)
{
  value tup = alloc_tuple(2);
  modify(&Field(tup, 0), Val_long(p->number));
  modify(&Field(tup, 1), copy_string((char *) p->name));
  return tup;
}

value copy_ptr_str_pair(const ptrstrpair_t *ps)
{
  value tup = alloc_tuple(2);
  modify(&Field(tup, 0), Val_long((unsigned int)(ps->p)));
  modify(&Field(tup, 1), copy_string((char *) ps->name));
  return tup;
}
