extern void **mosml_cptr_alloc(void);
extern void *mosml_cptr_free(void **cptr);
extern void *mosml_ffi_allocate(size_t size);
#ifdef FFI_MEM_WITH_REALLOC
  extern void *mosml_ffi_reallocate( void *oldptr, size_t old_size, size_t new_size );
#endif
extern void  mosml_ffi_free (void *p);
