#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>

#include "mosml/mlvalues.h"
#include "mosml/alloc.h"
#include "mosml/memory.h"
#include "mosml/fail.h"
#include "mosml/str.h"

value mmap_constants(value unit) {
  value res = alloc_tuple(5);
  Field(res, 0) = (value) MAP_FAILED;
  Field(res, 1) = Val_long(sysconf(_SC_PAGE_SIZE));
  Field(res, 2) = Val_long(MS_ASYNC);
  Field(res, 3) = Val_long(MS_SYNC);
  Field(res, 4) = Val_long(MS_INVALIDATE);
  return res;
}

value mmap_prot(value unit) {
  value res = alloc_tuple(4);
     Field(res, 0) = Val_long(PROT_EXEC);
     Field(res, 1) = Val_long(PROT_READ);
     Field(res, 2) = Val_long(PROT_WRITE);
     Field(res, 3) = Val_long(PROT_NONE);
  return res;
}

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS 0
#endif
#ifndef MAP_FIXED
#define MAP_FIXED 0
#endif
#ifndef MAP_GROWSDOWN
#define MAP_GROWSDOWN 0
#endif
#ifndef MAP_LOCKED
#define MAP_LOCKED 0
#endif
#ifndef MAP_HUGETLB
#define MAP_HUGETLB 0
#endif
#ifndef MAP_NONBLOCK
#define MAP_NONBLOCK 0
#endif
#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0
#endif
#ifndef MAP_POPULATE
#define MAP_POPULATE 0
#endif
#ifndef MAP_STACK
#define MAP_STACK 0
#endif
#ifndef MAP_UNINITIALIZED
#define MAP_UNINITIALIZED 0
#endif

value mmap_flags(value unit) {
  value res = alloc_tuple(12);
     Field(res, 0) = Val_long(MAP_SHARED);
     Field(res, 1) = Val_long(MAP_PRIVATE);
     Field(res, 2) = Val_long(MAP_ANONYMOUS);
     Field(res, 3) = Val_long(MAP_FIXED);
     Field(res, 4) = Val_long(MAP_GROWSDOWN);
     Field(res, 5) = Val_long(MAP_LOCKED);
     Field(res, 6) = Val_long(MAP_HUGETLB);
     Field(res, 7) = Val_long(MAP_NONBLOCK);
     Field(res, 8) = Val_long(MAP_NORESERVE);
     Field(res, 9) = Val_long(MAP_POPULATE);
     Field(res, 10) = Val_long(MAP_STACK);
     Field(res, 11) = Val_long(MAP_UNINITIALIZED);
  return res;
}

