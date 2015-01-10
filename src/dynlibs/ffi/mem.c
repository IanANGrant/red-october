#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* for hash table */
#include <glib.h>

#include "mffi.h"
#include "gc.h" /* For colour macros. */

/* Replacement memory management functions */

extern long jit_ffi_debug;
#define verbose_alloc jit_ffi_debug

static GHashTable *allocated_blocks = NULL;
static int block_cnt = 0;
static size_t bytes_allocated = 0;

#define ADD_BLOCK(p,s) g_hash_table_insert(allocated_blocks,p,(gpointer) s)
#define REMOVE_BLOCK(p) g_hash_table_remove(allocated_blocks,p)
#define ALLOCATED_BLOCK(p) g_hash_table_contains(allocated_blocks,p)
#define ALLOCATED_BLOCK_SIZE(p) ((size_t) g_hash_table_lookup(allocated_blocks,p))

static void init_alloc(void)
{
  if (allocated_blocks == NULL) {
    allocated_blocks = g_hash_table_new(&g_direct_hash, &g_direct_equal);
  }
}

typedef struct report_data_ {FILE *stream; size_t sum;} report_data_t;
  
static void printline_alloc(gpointer p, gpointer ps, report_data_t *data)
{
  fprintf(data->stream, "%p [%8u bytes]\n",p, (size_t) ps);
  data->sum += (size_t)ps;
}

void report_alloc(char *msg)
{
  report_data_t data = {.stream = stderr,.sum = 0};
  init_alloc();
  if (block_cnt) {
    fprintf(stderr,"%s\nAllocated blocks (%d) are:\n",msg,block_cnt);
     g_hash_table_foreach(allocated_blocks, (GHFunc)printline_alloc, (gpointer)&data);
     fprintf(stderr,"Total size = %u [%u] bytes in %d blocks.\n",data.sum,bytes_allocated,block_cnt);
  } else
    fprintf(stderr,"%s\nNo allocated blocks.\n",msg);
}

extern long jit_ffi_debug;

/* This is a wrapper around the system malloc, which lays the block out to look
   like a CAML runtime string, which is the representing type of Word8Vector in
   Moscow ML. The idea is that we can then recast static buffer pointers as 
   Word8Arrays. */

char *mosml_ffi_alloc (size_t size)
{
  char *hp;
  mlsize_t wosize = ((mlsize_t) size + sizeof (value)) / sizeof (value);
  mlsize_t offset_index;
  value result;
  int temp;
  mlsize_t temp2;
  mlsize_t temp3;

  adjust_gc_speed (size, MAX_FFI_ALLOC);

  if (jit_ffi_debug)
    printf("mosml_ffi_alloc: allocating %lu [%lu] bytes ...\n", Bhsize_wosize(wosize), (mlsize_t) size);

  hp = stat_alloc (Bhsize_wosize(wosize));

  if (jit_ffi_debug)
     printf("mosml_ffi_alloc: allocated %lu bytes at %p.\n", Bhsize_wosize(wosize), hp);

  Hd_hp (hp) = Make_header (wosize, String_tag, Black);
  result = Val_hp(hp);
  Field (result, wosize - 1) = 0;
  offset_index = Bsize_wsize (wosize) - 1;
  temp = (char) (offset_index - (mlsize_t) size);
  Byte (result, offset_index) = temp;
  temp2 = Bosize_val(result) - 1;
  temp3 = temp2 - Byte(result,temp2);

  if (temp3 != (mlsize_t) size)
    printf("\n\n*** HEAP CORRUPTION??? ***\n\n");

  if (jit_ffi_debug)
    printf("mosml_ffi_alloc: returning %p [offset_index = %lu,"
          " diff = %hhd, temp2=%lu, String.size=%lu]...\n",
           (char *) result, offset_index, temp, temp2, temp3);

  if (Byte (result, temp3) != 0)
    printf("\n\n*** ALLOC: HEAP CORRUPT: %p [Bhsize(hp)=%lu, String.size = %lu] ***\n\n\n",
           hp, Bhsize_hp(hp),temp3);

  return (char *) result;
}

char *mosml_ffi_alloc_wrap (void *buff, size_t size)
{
  char *hp;
  mlsize_t wosize = ((mlsize_t) size - sizeof(header_t)) / sizeof (value);
  mlsize_t offset_index;
  value result;
  int temp;
  mlsize_t temp2;
  mlsize_t temp3;

  if (jit_ffi_debug)
    printf("mosml_ffi_alloc_wrap: allocating %lu [%lu] bytes at %p ...\n", Bhsize_wosize(wosize), (mlsize_t) size, buff);

  hp = buff;

  if (jit_ffi_debug)
     printf("mosml_ffi_alloc_wrap: allocated %lu bytes at %p.\n", Bhsize_wosize(wosize), hp);

  Hd_hp (hp) = Make_header (wosize, String_tag, Black);
  result = Val_hp(hp);
  Field (result, wosize - 1) = 0;
  offset_index = Bsize_wsize (wosize) - 1;
  temp = (char) (offset_index - (mlsize_t) Bhsize_wosize(wosize));
  Byte (result, offset_index) = temp;
  temp2 = Bosize_val(result) - 1;
  temp3 = temp2 - Byte(result,temp2);

  if (temp3 != (mlsize_t) (mlsize_t) Bhsize_wosize(wosize))
    printf("\n\n*** mosml_ffi_alloc_wrap: HEAP CORRUPTION??? ***\n\n");

  if (jit_ffi_debug)
    printf("mosml_ffi_alloc_wrap: returning %p [offset_index = %lu,"
          " diff = %hhd, temp2=%lu, String.size=%lu]...\n",
           (char *) result, offset_index, temp, temp2, temp3);

  if (Byte (result, temp3) != 0)
    printf("\n\n*** mosml_ffi_alloc_wrap: HEAP CORRUPT: %p [Bhsize(hp)=%lu, String.size = %lu] ***\n\n\n",
           hp, Bhsize_hp(hp),temp3);

  return (char *) result;
}

#ifdef FFI_MEM_WITH_REALLOC
char *mosml_ffi_realloc (char *oldptr, size_t old_size, size_t new_size)
{
  char *hp = Hp_bp(oldptr);
  mlsize_t wosize = ((mlsize_t) new_size + sizeof (value)) / sizeof (value);
  mlsize_t offset_index;
  value result;

  adjust_gc_speed (new_size, MAX_FFI_ALLOC);

  if (jit_ffi_debug)
     printf("mosml_ffi_realloc: resizing from %lu to %lu bytes [%lu] ...\n",
	    Bhsize_hp(hp), Bhsize_wosize(wosize), (mlsize_t) new_size);

  hp = stat_resize (hp, Bhsize_wosize(wosize));

  if (jit_ffi_debug)
     printf("mosml_ffi_realloc: allocated %lu bytes at %p.\n", Bhsize_wosize(wosize), hp);

  Hd_hp (hp) = Make_header (wosize, String_tag, Black);
  result = Val_hp(hp);
  Field (result, wosize - 1) = 0;
  offset_index = Bsize_wsize (wosize) - 1;
  Byte (result, offset_index) = offset_index - (mlsize_t) new_size;

  return (char *) result;
}

char *mosml_ffi_resize (char *oldptr, size_t new_size)
{
  return mosml_ffi_realloc (oldptr, (size_t) 0, new_size);
}

#endif

void mosml_ffi_free (char *v)
{
  char *p = Hp_bp(v);
  mlsize_t temp2,temp3;

  temp2 = Bosize_val((value)v) - 1;
  temp3 = temp2 - Byte((value)v,temp2);

  if (Byte ((value)v, temp3) != 0)
    printf("\n\n*** FREE: HEAP CORRUPT: %p [Bhsize(p)=%lu, String.size = %lu] ***\n\n\n",
           p, Bhsize_hp(p),temp3);

  if (jit_ffi_debug)
    printf("mosml_ffi_free: freeing %p [Bhsize(p)=%lu, String.size = %lu]...\n", p, Bhsize_hp(p),temp3);

  stat_free (p);

  if (jit_ffi_debug)
    printf("mosml_ffi_free: freed %p.\n",p);
}

/* Protective memory management functions: principally to 
   stop lightning from freeing rogue pointers ... */

void *my_alloc(size_t size)
{
  void *p;

  init_alloc();
  p = mosml_ffi_alloc(size);
  if (verbose_alloc)
    fprintf(stderr,"ALLOC   %p[%8d]\n",p,size);

  if (p == NULL) {
    fprintf(stderr,"Out of memory (alloc(%d)).\n",size);
    report_alloc("(alloc)");
    exit(1);
  }
  assert(ADD_BLOCK(p,size));
  ++block_cnt;
  bytes_allocated += size;
  return p;
}

#ifdef FFI_MEM_WITH_REALLOC
void *my_realloc(void* p, size_t size)
{
  void *newp = NULL;

  init_alloc();

  if (!ALLOCATED_BLOCK(p)) {
    newp = my_alloc(size);
  } else {
    newp = mosml_ffi_resize(p,size);
    if (newp == NULL) {
      fprintf(stderr,"Out of memory (realloc(%p,%d)).\n",p,size);
      report_alloc("(realloc)");
      exit(1);
    }
    bytes_allocated -= ALLOCATED_BLOCK_SIZE(p);
    bytes_allocated += size;
    assert(REMOVE_BLOCK(p));
    assert(ADD_BLOCK(newp,size));
    if (verbose_alloc)
      fprintf(stderr,"REALLOC %p[%d] to %p[%d]\n",p,ALLOCATED_BLOCK_SIZE(p),newp,size);
  }
  return newp;
}
#endif

void my_free(void *p)
{
  init_alloc();
  if (verbose_alloc)
    fprintf(stderr,"FREE    %p[%8d]\n",p,ALLOCATED_BLOCK_SIZE(p));
  if (!ALLOCATED_BLOCK(p)) {
    fprintf(stderr,"*** free: Pointer %p was not allocated ***\n",p);
    // report_alloc("(free)");
  } else {
    bytes_allocated -= ALLOCATED_BLOCK_SIZE(p);
    assert(REMOVE_BLOCK(p));
    --block_cnt;
    mosml_ffi_free(p);
  }
}
