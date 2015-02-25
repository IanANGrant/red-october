#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "lightning.h"

#include "mosml/mlvalues.h"
#include "mosml/memory.h"
#include "mosml/alloc.h"
#include "mosml/major_gc.h"
#include "mosml/fail.h"

typedef int lgt_gpr_t;
typedef int lgt_fpr_t;
typedef unsigned int lgt_word_t;
typedef int lgt_int_t;

typedef double lgt_double_t;
typedef float lgt_float_t;

typedef void *lgt_ptr_t;
typedef jit_node_t *lgt_noderef_t;
typedef jit_state_t *lgt_state_t;

typedef int lgt_code_t;

struct intstrpair_ 
  {
    int number;
    const char *name;
  };

typedef struct intstrpair_ intstrpair_t;

#define EXTERNML

#define lgt_chk_args(v,n) \
           if (!(Is_block(v)) || !(Wosize_val(v) == n)) \
              failwith("Invalid argument")

#define Statep_val(x) ((lgt_state_t)(Field(x,1)))
#define Lcode_val(x) ((lgt_code_t)(Long_val(x)))

#if defined(__i386__) || defined(__x86_64__)
#define lightning_arch "x86"
#elif defined(__mips__)
#define lightning_arch "mips"
#elif defined(__arm__)
#define lightning_arch "arm"
#elif defined(__ppc__) || defined(__powerpc__)
#define lightning_arch "ppc"
#elif defined(__sparc__)
#define lightning_arch "sparc"
#elif defined(__ia64__)
#define lightning_arch "ia64"
#elif defined(__hppa__)
#define lightning_arch "hppa"
#elif defined(__aarch64__)
#define lightning_arch "aarch64"
#elif defined(__s390x__)
#define lightning_arch "s390x"
#endif
#if !defined(lightning_arch)
#define lightning_arch "undefined"
#endif

static const char *arch = lightning_arch;
static const int wordsize = __WORDSIZE;
static const int little_endian = __LITTLE_ENDIAN;
static const int big_endian = __BIG_ENDIAN;
static const int byte_order = __BYTE_ORDER;

/* ML programs need to be able to find the sizes of C data. */
static const intstrpair_t sizes[] = {
{sizeof(jit_int8_t),"int8"},
{sizeof(jit_uint8_t),"uint8"},
{sizeof(jit_int16_t),"int16"},
{sizeof(jit_uint16_t),"uint16"},
{sizeof(jit_int32_t),"int32"},
{sizeof(jit_uint32_t),"uint32t"},
{sizeof(jit_int64_t),"int64"},
{sizeof(jit_uint64_t),"uint64"},
{sizeof(jit_word_t),"word"},
{sizeof(jit_uword_t),"uword"},
{sizeof(jit_float32_t),"float32"},
{sizeof(jit_float64_t),"float64"},
{sizeof(jit_pointer_t),"pointer"},
{sizeof(jit_bool_t),"bool"},
{sizeof(jit_gpr_t),"gpr"},
{sizeof(jit_fpr_t),"fpr"}};

static int sizeslens = (int) (sizeof(sizes)) / (sizeof(struct intstrpair_));

/* These are some CPP macros lightning defines. We need to make
   them available as data to ML. */

static intstrpair_t constants[] = {
{jit_flag_node,"flag_node"},
{jit_flag_patch,"flag_patch"},
{jit_flag_data,"flag_data"},
{jit_flag_use,"flag_use"},
{jit_flag_head,"flag_head"},
{JIT_DISABLE_DATA,"DISABLE_DATA"},
{JIT_DISABLE_NOTE,"DISABLE_NOTE"},
{jit_class_chk,"class_chk"},
{jit_class_arg,"class_arg"},
{jit_class_sav,"class_sav"},
{jit_class_gpr,"class_gpr"},
{jit_class_fpr,"class_fpr"},
{JIT_HASH_CONSTS,"HASH_CONSTS"},
{JIT_NUM_OPERANDS,"NUM_OPERANDS"},
{JIT_FP,"FP"},
{JIT_R0,"R0"},
{JIT_R1,"R1"},
{JIT_R2,"R2"},
{JIT_V0,"V0"},
{JIT_V1,"V1"},
{JIT_V2,"V2"},
{0,"F0"},
{0,"F1"},
{0,"F2"},
{0,"F3"},
{0,"F4"},
{0,"F5"},
{0,"F6"},
{0,"F7"},
{0,"R_NUM"},
{0,"V_NUM"},
{0,"F_NUM"},
#if defined(JIT_FA0)
{0,"FA0"},
#endif
{JIT_NOREG,"NOREG"}};

/* Some of them are not constants: they depend on the CPU sub-type,
   determined at run time.  They will all perhaps need to be
   intialised at run time, because they could break with different
   architectures, or 'internal' lightning changes .*/

value jit_initialise_constants(value dummy) {
  intstrpair_t *consts = &constants[0];
  
  while (strcmp(consts->name, "F0"))
      consts++;

  consts[0].number = JIT_F0;
  consts[1].number = JIT_F1;
  consts[2].number = JIT_F2;
  consts[3].number = JIT_F3;
  consts[4].number = JIT_F4;
  consts[5].number = JIT_F5;
  consts[6].number = JIT_F6;
  consts[7].number = JIT_F7;
  consts[8].number = JIT_R_NUM;
  consts[9].number = JIT_V_NUM;
  consts[10].number = JIT_F_NUM;
#if defined(JIT_FA0)
  consts[11].number = JIT_FA0;
#endif
  return Val_unit;
}

static int constantslens = (int) (sizeof(constants)) / (sizeof(struct intstrpair_));

value sml_init_jit(value unit) {
  /* CAMLRUNPATH is defined on the cc cmd line */
  init_jit(CAMLRUNPATH);

  return Val_unit;
}

value sml_finish_jit(value unit) {
  finish_jit();
  return Val_unit;
}

value jit_r_     (value i)        { return Val_long(jit_r(Long_val(i)));};
value jit_r_num_ (value unit)     { return Val_long(jit_r_num());};
value jit_v_     (value i)        { return Val_long(jit_v(Long_val(i)));};
value jit_v_num_ (value unit)     { return Val_long(jit_v_num());};
value jit_f_     (value i)        { return Val_long(jit_f(Long_val(i)));};
value jit_f_num_ (value unit)     { return Val_long(jit_f_num());};

#if defined(jit_sse_reg_p)
int jit_sse_reg_p_(int reg) { return (jit_sse_reg_p(reg));};
#endif

value jit_get_constantslength(value unit)
{
  return Val_long(constantslens);
}

static value copy_int_str_pair(const intstrpair_t *p)
{
  value tup = alloc_tuple(2);
  modify(&Field(tup, 0), Val_long(p->number));
  modify(&Field(tup, 1), copy_string((char *) p->name));
  return tup;
}

value jit_get_constantsentry(value num)
{   int entry = Long_val(num);

    if (entry < 0 || entry >= constantslens)
       failwith("Invalid constantsentry index");

    return copy_int_str_pair(&constants[entry]);
}

value jit_get_sizeslength(value unit)
{
  return Val_long(sizeslens);
}

value jit_get_sizesentry(value num)
{   int entry = Long_val(num);

    if (entry < 0 || entry >= sizeslens)
       failwith("Invalid sizesentry index");

    return copy_int_str_pair(&sizes[entry]);
}

value jit_get_NULL(value unit)
{
  return (value) NULL;
}

value jit_get_arch(value unit)
{
  return copy_string((char *) arch);
}

value jit_get_little_endian(value unit)
{
  return Val_long(little_endian);
}

value jit_get_big_endian(value unit)
{
  return Val_long(big_endian);
}

value jit_get_byteorder(value unit)
{
  return Val_long(byte_order);
}

value jit_get_wordsize(value unit)
{
  return Val_long(wordsize);
}

static void lgt_state_finalize(value v)
{ 
  lgt_state_t sp;

  sp = Statep_val(v);

  _jit_destroy_state(sp);
}

static value lgt_state_alloc(lgt_state_t sp)
{ 
  value result = alloc_final(2, &lgt_state_finalize, 1, 100);
  initialize(&Field(result, 1), (value) sp);
  return result;
}

static value lgt_new_state(lgt_state_t sp) { 
  return lgt_state_alloc(sp);
}

static lgt_state_t lgt_state_val(value v) {
  return Statep_val(v);
}

static lgt_code_t lgt_code_val(value v) {
  return Lcode_val(v);
}

static lgt_gpr_t lgt_gpr_val(value v) {
  lgt_gpr_t reg;

  reg = (lgt_gpr_t) Long_val(v);

  if ((!(reg >= JIT_R0 && reg <= JIT_R0 + JIT_R_NUM - 1)) &&
      (!(reg >= JIT_V0 && reg <= JIT_V0 + JIT_V_NUM - 1)) &&
      (!(reg == JIT_FP)))
    failwith("Invalid GPR number");

  return reg;
}

static lgt_fpr_t lgt_fpr_val(value v) {
  lgt_fpr_t reg;

  reg = (lgt_fpr_t) Long_val(v);

  if (!(reg >= JIT_F0 && reg <= JIT_F0 + JIT_F_NUM - 1))
     failwith("Invalid FPR number");

  return reg;
}

static lgt_ptr_t lgt_ptr_val(value v) { 
  if (Is_in_heap(v))
    failwith("pointer is a heap object");

  return (lgt_ptr_t) v;
}

static lgt_word_t lgt_ptrc_val(value v) { 
  if (Is_in_heap(v))
    failwith("pointer is a heap object");

  return (lgt_word_t) v;
}

static lgt_word_t lgt_word_val(value v) {
  if (!Is_long(v))
    failwith("Invalid word argument");

  return (lgt_word_t) (Long_val(v));
}

static lgt_noderef_t lgt_noderef_val(value v) { 
  if (Is_in_heap(v))
    failwith("noderef is a heap object");

  return (lgt_noderef_t) v;
}

static lgt_float_t lgt_float_val(value v) { 
  if (Tag_val(v) != Double_tag)
    failwith("Invalid float argument");

  return (lgt_float_t) (Double_val(v));
}

static lgt_double_t lgt_double_val(value v) { 
  if (Tag_val(v) != Double_tag)
    failwith("Invalid float argument");

  return (lgt_double_t) (Double_val(v));
}

#define Val_lgt_state(x)   ((value) lgt_new_state(x))
#define Val_lgt_ptr(x)     ((value) x)
#define Val_lgt_noderef(x) ((value) x)
#define Val_lgt_int(x)     (Val_int(x))

EXTERNML value sml_jit_new_state(value unit) {
  value result;

  result = Val_lgt_state(jit_new_state());

  return result;
}

/*
  jit_realize();
       jit_get_data(NULL, NULL);
       jit_set_data(NULL, 0, JIT_DISABLE_DATA | JIT_DISABLE_NOTE);
       ...



       jit_uint8_t   *code;
       int           *(func)(int);     
       jit_word_t     code_size;
       jit_word_t     real_code_size;
       ...
       jit_realize();                  
       jit_get_code(&code_size);       
       code_size = (code_size + 4095) & -4096;
       do (;;) {
         code = mmap(NULL, code_size, PROT_EXEC | PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON, -1, 0);
         jit_set_code(code, code_size);
         if ((func = jit_emit()) == NULL) {
           munmap(code, code_size);
           code_size += 4096;
         }
       } while (func == NULL);
       jit_get_code(&real_code_size);  

*/

/* Read in generated code */

#if __WORDSIZE == 32
#include "lgt32.c"
#else
#include "lgt64.c"
#endif
