#include <stdio.h>
#include <string.h>

#include "mosml/mlvalues.h"
#include "mosml/alloc.h"
#include "mosml/major_gc.h"
#include "mosml/memory.h"
#include "mosml/fail.h"
#include "mosml/str.h"

value heap_constants(value unit) {
  value res = alloc_tuple(9);
  Field(res, 0) = Val_long(Num_tags);
  Field(res, 1) = Val_long(No_scan_tag);
  Field(res, 2) = Val_long(Weak_tag);
  Field(res, 3) = Val_long(String_tag);
  Field(res, 4) = Val_long(Reference_tag);
  Field(res, 5) = Val_long(Double_tag);
  Field(res, 6) = Val_long(Abstract_tag);
  Field(res, 7) = Val_long(Final_tag);
  Field(res, 8) = Val_long(Closure_tag);
  return res;
}

value heap_Is_in_heap (char *cptr) {
  return (Atom(Is_in_heap (cptr) ? 1 : 0));
}

value heap_Atom (value tag) {
  return (Atom((int)Long_val(tag)));
}

value heap_Is_atom(value obj) {
  return (Atom(Is_atom (obj) ? 1 : 0));
}

