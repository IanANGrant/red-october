#include <stdio.h>

#include "mffi.h"

extern long jit_ffi_debug;

void *mosml_ffi_alloc( size_t size )
{
  void *p;
  adjust_gc_speed( size, MAX_FFI_ALLOC );
  p = stat_alloc( size );
  if (jit_ffi_debug)
    printf("mosml_ffi_alloc: allocated %d bytes at %p.\n",size, p);
  return p;
}

#ifdef FFI_MEM_WITH_REALLOC
void *mosml_ffi_realloc( void *oldptr, size_t old_size, size_t new_size )
{
  #pragma unused (old_size)
  adjust_gc_speed( new_size, MAX_FFI_ALLOC );
  return stat_resize( oldptr, new_size );
}
#endif

void mosml_ffi_free (void *p)
{
  stat_free(p);
  if (jit_ffi_debug)
    printf("mosml_ffi_free: freed %p.\n",p);
}
