 struct intstrtriple_ 
  {
    mlsize_t offset;
    mlsize_t length;
    const char *name;
  };

typedef struct intstrtriple_ intstrtriple_t;

struct intstrpair_ 
  {
    int number;
    const char *name;
  };

typedef struct intstrpair_ intstrpair_t;

struct codestrpair_ 
  {
    const jit_code_t code;
    const char *name;
  };

typedef struct codestrpair_ codestrpair_t;

struct ptrstrpair_ 
  {
    const void *p;
    const char *name;
  };

typedef struct ptrstrpair_ ptrstrpair_t;

#if !defined(__NO_DECL)
extern value copy_int_str_triple(const intstrtriple_t *p);
extern value copy_int_str_pair(const intstrpair_t *p);
extern value copy_ptr_str_pair(const ptrstrpair_t *p);
#endif


