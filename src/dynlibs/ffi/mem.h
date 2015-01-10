extern char* mosml_ffi_alloc (size_t size);
extern char* mosml_ffi_alloc_wrap (void *buff, size_t size);
extern void  mosml_ffi_free (char *p);

extern void* my_alloc (size_t size);
extern void  my_free (void *p);

extern void report_alloc(char *);

#ifdef FFI_MEM_WITH_REALLOC
extern char *mosml_ffi_realloc (char *oldptr, size_t old_size, size_t new_size);
extern char *mosml_ffi_resize (char *oldptr, size_t new_size);
extern void *my_realloc (void *oldptr, size_t new_size);
#endif
