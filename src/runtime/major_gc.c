#include <stdlib.h>
#include <stdint.h>
#include "config.h"
#include "debugger.h"
#include "fail.h"
#include "freelist.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "globals.h"
#include "major_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"

#ifdef macintosh
#include <Memory.h>
#endif

#include "runtime.h"

#ifdef ANSI
#include <limits.h>
#else
#ifdef SIXTYFOUR
#define LONG_MAX 0x7FFFFFFFFFFFFFFF
#else
#define LONG_MAX 0x7FFFFFFF
#endif
#endif

int percent_free;
long major_heap_increment;
char *heap_start, *heap_end;
char *page_table;
asize_t page_table_size;
char *gc_sweep_hp;
int gc_phase;

typedef struct {
  intptr_t low, high;
} p_table_entry;

static p_table_entry *p_table;
static size_t p_table_total_size;
static size_t p_table_current_size;

void p_table_init(size_t initial) {
  p_table = malloc(initial*sizeof(p_table_entry));
  if(p_table == NULL)
      fatal_error ("No room for allocating page table\n");
  p_table_total_size = initial;
  p_table_current_size = 0;
}

#define RawPage(p) (((intptr_t) (p)) >> Page_log)

char p_table_in_heap_simple(addr a) {
  int i;
  intptr_t p = RawPage(a);
  for(i = 0; i < p_table_current_size; i++) {
    //printf("p: %u low: %u high: %u\n", p, p_table[i].low, p_table[i].high);
    if(p_table[i].low <= p && p < p_table[i].high) {
      return In_heap;
    }
  }
  return Not_in_heap;
}

char p_table_in_heap_16(addr a) {
  intptr_t p = RawPage(a);
  int i = 0;
  while(i + 15 < p_table_current_size) {
    if(   (p_table[i].low <= p && p < p_table[i].high)
       || (p_table[i + 1].low <= p && p < p_table[i + 1].high)
       || (p_table[i + 2].low <= p && p < p_table[i + 2].high)
       || (p_table[i + 3].low <= p && p < p_table[i + 3].high)
       || (p_table[i + 4].low <= p && p < p_table[i + 4].high)
       || (p_table[i + 5].low <= p && p < p_table[i + 5].high)
       || (p_table[i + 6].low <= p && p < p_table[i + 6].high)
       || (p_table[i + 7].low <= p && p < p_table[i + 7].high)
       || (p_table[i + 8].low <= p && p < p_table[i + 8].high)
       || (p_table[i + 9].low <= p && p < p_table[i + 9].high)
       || (p_table[i + 10].low <= p && p < p_table[i + 10].high)
       || (p_table[i + 11].low <= p && p < p_table[i + 11].high)
       || (p_table[i + 12].low <= p && p < p_table[i + 12].high)
       || (p_table[i + 13].low <= p && p < p_table[i + 13].high)
       || (p_table[i + 14].low <= p && p < p_table[i + 14].high)
       || (p_table[i + 15].low <= p && p < p_table[i + 15].high)
       ) return In_heap;
    i += 16;
  }
  switch(p_table_current_size - i) {
  case 15:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 14:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 13:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 12:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 11:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 10:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 9:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 8:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 7:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 6:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 5:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 4:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 3:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 2:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 1:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  default: return Not_in_heap;
  }
}

char p_table_in_heap(addr a) {
  intptr_t p = RawPage(a);
  int i = 0;
  while(i + 63 < p_table_current_size) {
    if(   (p_table[i].low <= p && p < p_table[i].high)
       || (p_table[i + 1].low <= p && p < p_table[i + 1].high)
       || (p_table[i + 2].low <= p && p < p_table[i + 2].high)
       || (p_table[i + 3].low <= p && p < p_table[i + 3].high)
       || (p_table[i + 4].low <= p && p < p_table[i + 4].high)
       || (p_table[i + 5].low <= p && p < p_table[i + 5].high)
       || (p_table[i + 6].low <= p && p < p_table[i + 6].high)
       || (p_table[i + 7].low <= p && p < p_table[i + 7].high)
       || (p_table[i + 8].low <= p && p < p_table[i + 8].high)
       || (p_table[i + 9].low <= p && p < p_table[i + 9].high)
       || (p_table[i + 10].low <= p && p < p_table[i + 10].high)
       || (p_table[i + 11].low <= p && p < p_table[i + 11].high)
       || (p_table[i + 12].low <= p && p < p_table[i + 12].high)
       || (p_table[i + 13].low <= p && p < p_table[i + 13].high)
       || (p_table[i + 14].low <= p && p < p_table[i + 14].high)
       || (p_table[i + 15].low <= p && p < p_table[i + 15].high)
       || (p_table[i + 16].low <= p && p < p_table[i + 16].high)
       || (p_table[i + 17].low <= p && p < p_table[i + 17].high)
       || (p_table[i + 18].low <= p && p < p_table[i + 18].high)
       || (p_table[i + 19].low <= p && p < p_table[i + 19].high)
       || (p_table[i + 20].low <= p && p < p_table[i + 20].high)
       || (p_table[i + 21].low <= p && p < p_table[i + 21].high)
       || (p_table[i + 22].low <= p && p < p_table[i + 22].high)
       || (p_table[i + 23].low <= p && p < p_table[i + 23].high)
       || (p_table[i + 24].low <= p && p < p_table[i + 24].high)
       || (p_table[i + 25].low <= p && p < p_table[i + 25].high)
       || (p_table[i + 26].low <= p && p < p_table[i + 26].high)
       || (p_table[i + 27].low <= p && p < p_table[i + 27].high)
       || (p_table[i + 28].low <= p && p < p_table[i + 28].high)
       || (p_table[i + 29].low <= p && p < p_table[i + 29].high)
       || (p_table[i + 30].low <= p && p < p_table[i + 30].high)
       || (p_table[i + 31].low <= p && p < p_table[i + 31].high)
       || (p_table[i + 32].low <= p && p < p_table[i + 32].high)
       || (p_table[i + 33].low <= p && p < p_table[i + 33].high)
       || (p_table[i + 34].low <= p && p < p_table[i + 34].high)
       || (p_table[i + 35].low <= p && p < p_table[i + 35].high)
       || (p_table[i + 36].low <= p && p < p_table[i + 36].high)
       || (p_table[i + 37].low <= p && p < p_table[i + 37].high)
       || (p_table[i + 38].low <= p && p < p_table[i + 38].high)
       || (p_table[i + 39].low <= p && p < p_table[i + 39].high)
       || (p_table[i + 40].low <= p && p < p_table[i + 40].high)
       || (p_table[i + 41].low <= p && p < p_table[i + 41].high)
       || (p_table[i + 42].low <= p && p < p_table[i + 42].high)
       || (p_table[i + 43].low <= p && p < p_table[i + 43].high)
       || (p_table[i + 44].low <= p && p < p_table[i + 44].high)
       || (p_table[i + 45].low <= p && p < p_table[i + 45].high)
       || (p_table[i + 46].low <= p && p < p_table[i + 46].high)
       || (p_table[i + 47].low <= p && p < p_table[i + 47].high)
       || (p_table[i + 48].low <= p && p < p_table[i + 48].high)
       || (p_table[i + 49].low <= p && p < p_table[i + 49].high)
       || (p_table[i + 50].low <= p && p < p_table[i + 50].high)
       || (p_table[i + 51].low <= p && p < p_table[i + 51].high)
       || (p_table[i + 52].low <= p && p < p_table[i + 52].high)
       || (p_table[i + 53].low <= p && p < p_table[i + 53].high)
       || (p_table[i + 54].low <= p && p < p_table[i + 54].high)
       || (p_table[i + 55].low <= p && p < p_table[i + 55].high)
       || (p_table[i + 56].low <= p && p < p_table[i + 56].high)
       || (p_table[i + 57].low <= p && p < p_table[i + 57].high)
       || (p_table[i + 58].low <= p && p < p_table[i + 58].high)
       || (p_table[i + 59].low <= p && p < p_table[i + 59].high)
       || (p_table[i + 60].low <= p && p < p_table[i + 60].high)
       || (p_table[i + 61].low <= p && p < p_table[i + 61].high)
       || (p_table[i + 62].low <= p && p < p_table[i + 62].high)
       || (p_table[i + 63].low <= p && p < p_table[i + 63].high)
       ) return In_heap;
    i += 64;
  }
  switch(p_table_current_size - i) {
  case 63:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 62:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 61:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 60:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 59:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 58:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 57:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 56:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 55:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 54:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 53:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 52:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 51:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 50:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 49:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 48:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 47:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 46:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 45:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 44:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 43:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 42:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 41:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 40:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 39:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 38:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 37:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 36:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 35:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 34:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 33:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 32:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 31:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 30:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 29:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 28:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 27:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 26:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 25:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 24:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 23:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 22:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 21:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 20:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 19:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 18:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 17:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 16:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 15:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 14:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 13:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 12:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 11:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 10:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 9:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 8:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 7:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 6:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 5:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 4:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 3:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 2:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  case 1:
    if(p_table[i].low <= p && p < p_table[i].high) return In_heap;
    i++;
  default: return Not_in_heap;
  }
}


void p_table_update_size() {
  p_table_total_size *= 2;
  p_table = realloc(p_table, sizeof(p_table_entry)*p_table_total_size);
  if(p_table == NULL)
    fatal_error("No memory for page table");
  gc_message ("Growing p_table to %ld\n", p_table_total_size);

}

void p_table_add_pages(addr start, addr end) {
  intptr_t s, e;
  if(p_table_current_size == p_table_total_size)
    p_table_update_size();
  p_table[p_table_current_size].low = RawPage(start);
  p_table[p_table_current_size].high = RawPage(end);
  p_table_current_size++;
}



/* The mark phase will register pointers to live arrays of weak
   pointers in weak_arrays.  Then the weak phase traverses each weak
   array and resets pointers to objects that will be deallocated by the
   sweep phase: 
*/

static value *weak_arrays;
value *weak_arrays_cur, *weak_arrays_end;
static asize_t weak_arrays_size;

static value *gray_vals;
value *gray_vals_cur, *gray_vals_end;
static asize_t gray_vals_size;
static int heap_is_pure;   /* The heap is pure if the only gray objects
                              below [markhp] are also in [gray_vals]. */
unsigned long allocated_words;
unsigned long extra_heap_memory;
extern char *fl_merge;  /* Defined in freelist.c. */

static char *markhp, *chunk, *limit;

static void realloc_gray_vals (void)
{
  value *new;

  Assert (gray_vals_cur == gray_vals_end);
  if (gray_vals_size < stat_heap_size / 128){
    gc_message ("Growing gray_vals to %ldk\n",
		(long) gray_vals_size * sizeof (value) / 512);
    new = (value *) realloc ((char *) gray_vals,
                             2 * gray_vals_size * sizeof (value));
    if (new == NULL){
      gc_message ("No room for growing gray_vals\n", 0);
      gray_vals_cur = gray_vals;
      heap_is_pure = 0;
    }else{
      gray_vals = new;
      gray_vals_cur = gray_vals + gray_vals_size;
      gray_vals_size *= 2;
      gray_vals_end = gray_vals + gray_vals_size;
    }
  }else{
    gray_vals_cur = gray_vals + gray_vals_size / 2;
    heap_is_pure = 0;
  }
}

static void realloc_weak_arrays (void)
{
  value *new;

  Assert (weak_arrays_cur == weak_arrays_end);
  gc_message ("Growing weak_arrays to %ld\n",
              (long) weak_arrays_size * 2);
  new = (value *) realloc ((char *) weak_arrays,
                             2 * weak_arrays_size * sizeof (value));
  if (new == NULL){
    fatal_error ("Fatal error: cannot grow weak_arrays table.\n");
  }else{
    weak_arrays = new;
    weak_arrays_cur = weak_arrays + weak_arrays_size;
    weak_arrays_size *= 2;
    weak_arrays_end = weak_arrays + weak_arrays_size;
  }
}

void darken (value v)
{
  if (Is_block (v) && Is_in_heap (v) && Is_white_val (v)){
    Hd_val (v) = Grayhd_hd (Hd_val (v));
    *gray_vals_cur++ = v;
    if (gray_vals_cur >= gray_vals_end) realloc_gray_vals ();
  }
}

static void darken_root (value *p, value v)
{
  darken (v);
}

static void start_cycle (void)
{
  Assert (gray_vals_cur == gray_vals);
  Assert (Is_white_val (global_data));
  darken (global_data);
  local_roots (darken_root);
  gc_phase = Phase_mark;
  markhp = NULL;
}

static void mark_slice (long work)
{
  value v, child;
  mlsize_t i;

  while (work > 0){
    if (gray_vals_cur > gray_vals){
      v = *--gray_vals_cur;
      Assert (Is_gray_val (v));
      Hd_val (v) = Blackhd_hd (Hd_val (v));
      if (Tag_val (v) < No_scan_tag){
	for (i = Wosize_val (v); i > 0;){
	  --i;
	  child = Field (v, i);
	  darken (child);
	}
      } else if (Tag_val(v) == Weak_tag) {
        *weak_arrays_cur++ = v;
        if (weak_arrays_cur >= weak_arrays_end) realloc_weak_arrays ();
      }
      work -= Whsize_val (v);
    }else if (markhp != NULL){
      if (markhp == limit){
	chunk = (((heap_chunk_head *) chunk) [-1]).next;
	if (chunk == NULL){
	  markhp = NULL;
	}else{
	  markhp = chunk;
	  limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
	}
      }else{
	if (Is_gray_val (Val_hp (markhp))){
	  Assert (gray_vals_cur == gray_vals);
	  *gray_vals_cur++ = Val_hp (markhp);
	}
	markhp += Bhsize_hp (markhp);
      }
    }else if (!heap_is_pure){
      heap_is_pure = 1;
      chunk = heap_start;
      markhp = chunk;
      limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
    }else{
      /* Marking is done. */
      gc_sweep_hp = heap_start;
      fl_init_merge ();
      gc_phase = Phase_weak;
      chunk = heap_start;
      gc_sweep_hp = chunk;
      limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
      work = 0;
    }
  }
}

/* Reset weak pointers to objects that will be deallocated by the sweep phase
 */

static void weak_phase()
{
  value *c;
  for (c = weak_arrays; c < weak_arrays_cur; c++)
    { 
      int i;
      value arr = *c;
      int len = Wosize_val(arr);
      for (i=0; i < len; i++) 
	{ 
	  value v = Field(arr, i);
	  if (Is_block(v) && Is_in_heap(v) && Is_white_val(v))
	    Field(arr, i) = (value)NULL;
	}
    }
  weak_arrays_cur = weak_arrays;
  gc_phase = Phase_sweep;
}

static void sweep_slice (long work)
{
  char *hp;
  header_t hd;

  while (work > 0){
    if (gc_sweep_hp < limit){
      hp = gc_sweep_hp;
      hd = Hd_hp (hp);
      work -= Whsize_hd (hd);
      gc_sweep_hp += Bhsize_hd (hd);
      switch (Color_hd (hd)){
      case White:
	if (Tag_hd (hd) == Final_tag){
	  Final_fun (Val_hp (hp)) (Val_hp (hp));
	}
	gc_sweep_hp = fl_merge_block (Bp_hp (hp));
	break;
      case Gray:
	Assert (0);     /* Fall through to Black when not in debug mode. */
      case Black:
	Hd_hp (hp) = Whitehd_hd (hd);
	break;
      case Blue:
	/* Only the blocks of the free-list are blue.  See [freelist.c]. */
	fl_merge = Bp_hp (hp);
	break;
      }
      Assert (gc_sweep_hp <= limit);
    }else{
      chunk = (((heap_chunk_head *) chunk) [-1]).next;
      if (chunk == NULL){
	/* Sweeping is done.  Start the next cycle. */
        ++ stat_major_collections;
	work = 0;
	start_cycle ();
      }else{
	gc_sweep_hp = chunk;
	limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
      }
    }
  }
}

void major_collection_slice (void)
{
  /* Free memory at the start of the GC cycle:
                 FM = stat_heap_size * percent_free / 100 * 2/3
     Proportion of free memory consumed since the previous slice:
                 PH = allocated_words / FM
     Proportion of extra-heap memory consumed since the previous slice:
                 PE = extra_heap_memory / stat_heap_size
     Proportion of total work to do in this slice:
                 P  = PH + PE
     Amount of marking work for the GC cycle:
                 MW = stat_heap_size * (100 - percent_free) / 100
     Amount of sweeping work for the GC cycle:
                 SW = stat_heap_size
     Amount of marking work for this slice:
                 MS = MW * 2 * P
                 MS = 2 * (100 - percent_free)
                      * (allocated_words * 3 / percent_free / 2
		         + 100 * extra_heap_memory)
     Amount of sweeping work for this slice:
                 SS = SW * 2 * P
                 SS = 2 * 100
		      * (allocated_words * 3 / percent_free / 2
		         + 100 * extra_heap_memory)
     This slice will either mark MS words or sweep SS words.
  */

#define Margin 100  /* Make it a little faster to be on the safe side. */

  if (gc_phase == Phase_mark){
    mark_slice (2 * (100 - percent_free)
		* (allocated_words * 3 / percent_free / 2
                   + 100 * extra_heap_memory)
		+ Margin);
    gc_message ("!", 0);
  }else if (gc_phase == Phase_weak){  
    weak_phase();
    gc_message (".", 0);
  }else{
    Assert (gc_phase == Phase_sweep);
    sweep_slice (200 * (allocated_words * 3 / percent_free / 2
			+ 100 * extra_heap_memory)
		 + Margin);
    gc_message ("$", 0);
  }
  stat_major_words += allocated_words;
  allocated_words = 0;
  extra_heap_memory = 0;
}

/* The minor heap must be empty when this function is called. */
void finish_major_cycle (void)
{

  beg_gc_time();

  if (gc_phase == Phase_mark) mark_slice (LONG_MAX);
  if (gc_phase == Phase_weak) weak_phase();
  Assert (gc_phase == Phase_sweep);
  sweep_slice (LONG_MAX);
  stat_major_words += allocated_words;
  allocated_words = 0;

  end_gc_time();

}

asize_t round_heap_chunk_size (asize_t request)
{                            Assert (major_heap_increment >= Heap_chunk_min);
  if (request < major_heap_increment){
                              Assert (major_heap_increment % Page_size == 0);
    return major_heap_increment;
  }else if (request <= Heap_chunk_max){
    return ((request + Page_size - 1) >> Page_log) << Page_log;
  }else{
    raise_out_of_memory ();
  }
  return 0;			/* Can't reach return */
}

void init_major_heap (asize_t heap_size)
{
  asize_t i;

  stat_heap_size = round_heap_chunk_size (heap_size);
  Assert (stat_heap_size % Page_size == 0);
  heap_start = aligned_malloc (stat_heap_size + sizeof (heap_chunk_head),
			       sizeof (heap_chunk_head));
  if (heap_start == NULL)
    fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  heap_start += sizeof (heap_chunk_head);
  Assert ((unsigned long) heap_start % Page_size == 0);
  (((heap_chunk_head *) heap_start) [-1]).size = stat_heap_size;
  (((heap_chunk_head *) heap_start) [-1]).next = NULL;
  heap_end = heap_start + stat_heap_size;
  Assert ((unsigned long) heap_end % Page_size == 0);
#ifdef SIXTEEN
  page_table_size = 640L * 1024L / Page_size + 1;
#else
  page_table_size = 4 * stat_heap_size / Page_size;
#endif
  /*  page_table = (char *) malloc (page_table_size);
  if (page_table == NULL){
    fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  }
  for (i = 0; i < page_table_size; i++){
    page_table [i] = Not_in_heap;
  }
  for (i = Page (heap_start); i < Page (heap_end); i++){
    page_table [i] = In_heap;
  }
  */
  //  p_table_init(page_table_size);
  p_table_init(64);
  p_table_add_pages(heap_start, heap_end);
  Hd_hp (heap_start) = Make_header (Wosize_bhsize (stat_heap_size), 0, Blue);
  fl_init_merge ();
  fl_merge_block (Bp_hp (heap_start));
  /* We start the major GC in the marking phase, just after the roots have been
     darkened. (Since there are no roots, we don't have to darken anything.) */
  gc_phase = Phase_mark;
  weak_arrays_size = 1;
  weak_arrays = (value *) malloc (weak_arrays_size * sizeof (value));
  weak_arrays_cur = weak_arrays;
  weak_arrays_end = weak_arrays + weak_arrays_size;
  gray_vals_size = 2048;
  gray_vals = (value *) malloc (gray_vals_size * sizeof (value));
  gray_vals_cur = gray_vals;
  gray_vals_end = gray_vals + gray_vals_size;
  heap_is_pure = 1;
  allocated_words = 0;
  extra_heap_memory = 0;
}
