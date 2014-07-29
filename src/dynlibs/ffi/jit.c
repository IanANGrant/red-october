#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <lightning.h>

#include "mffi.h"
#include "mem.h"
#include "types.h"

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

static void init_constants(void) {
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
  return;
}

static int constantslens = (int) (sizeof(constants)) / (sizeof(struct intstrpair_));

/* The following are some of the CPP C macros lightning defines, we need to make them
   available as functions callable from ML. */
#if defined(jit_class)
int jit_class_(int reg) {jit_class(reg);};
#endif
#if defined(jit_regno)
int jit_regno_(int reg) {jit_regno(reg);};
#endif
#if defined(jit_sse2_p)
int jit_sse_2_p(void) { return (jit_sse2_p());};
#endif
#if defined(jit_x87_reg_p)
int jit_x_87_reg_p(int reg) { return (jit_x87_reg_p(reg));};
#endif
#if defined(jit_arg_reg_p)
int jit_arg_reg_p_(int i) { return (jit_arg_reg_p(i));};
#endif
#if defined(jit_r)
int jit_r_ (int i)        { return (jit_r(i));};
#endif
#if defined(jit_r_num)
int jit_r_num_(void)      { return (jit_r_num());};
#endif
#if defined(jit_sse_reg_p)
int jit_sse_reg_p_(int reg) { return (jit_sse_reg_p(reg));};
#endif
#if defined(jit_v)
int jit_v_ (int i)        { return (jit_v(i));};
#endif
#if defined(jit_v_num)
int jit_v_num_(void)      { return (jit_v_num());};
#endif
#if defined(jit_f)
int jit_f_ (int i)        { return (jit_f(i));};
#endif
#if defined(jit_f_num)
int jit_f_num_(void)      { return (jit_f_num());};
#endif
#if defined(jit_arg_f_reg_p)
int jit_arg_f_reg_p_ (int i) { return (jit_arg_f_reg_p(i));};
#endif

/* This is an enumeration of codes for lightning functions. We need to
   make sure that the ML code and the lightning library are using the
   same values. */

const codestrpair_t codetable[] = {
{jit_code_data, "data"},
{jit_code_live, "live"},
{jit_code_save, "save"},
{jit_code_load, "load"},
{jit_code_name, "name"},
{jit_code_note, "note"},
{jit_code_label, "label"},
{jit_code_prolog, "prolog"},
{jit_code_arg, "arg"},
{jit_code_addr, "addr"},
{jit_code_addi, "addi"},
{jit_code_addcr, "addcr"},
{jit_code_addci, "addci"},
{jit_code_addxr, "addxr"},
{jit_code_addxi, "addxi"},
{jit_code_subr, "subr"},
{jit_code_subi, "subi"},
{jit_code_subcr, "subcr"},
{jit_code_subci, "subci"},
{jit_code_subxr, "subxr"},
{jit_code_subxi, "subxi"},
{jit_code_mulr, "mulr"},
{jit_code_muli, "muli"},
{jit_code_qmulr, "qmulr"},
{jit_code_qmuli, "qmuli"},
{jit_code_qmulr_u, "qmulr_u"},
{jit_code_qmuli_u, "qmuli_u"},
{jit_code_divr, "divr"},
{jit_code_divi, "divi"},
{jit_code_divr_u, "divr_u"},
{jit_code_divi_u, "divi_u"},
{jit_code_qdivr, "qdivr"},
{jit_code_qdivi, "qdivi"},
{jit_code_qdivr_u, "qdivr_u"},
{jit_code_qdivi_u, "qdivi_u"},
{jit_code_remr, "remr"},
{jit_code_remi, "remi"},
{jit_code_remr_u, "remr_u"},
{jit_code_remi_u, "remi_u"},
{jit_code_andr, "andr"},
{jit_code_andi, "andi"},
{jit_code_orr, "orr"},
{jit_code_ori, "ori"},
{jit_code_xorr, "xorr"},
{jit_code_xori, "xori"},
{jit_code_lshr, "lshr"},
{jit_code_lshi, "lshi"},
{jit_code_rshr, "rshr"},
{jit_code_rshi, "rshi"},
{jit_code_rshr_u, "rshr_u"},
{jit_code_rshi_u, "rshi_u"},
{jit_code_negr, "negr"},
{jit_code_comr, "comr"},
{jit_code_ltr, "ltr"},
{jit_code_lti, "lti"},
{jit_code_ltr_u, "ltr_u"},
{jit_code_lti_u, "lti_u"},
{jit_code_ler, "ler"},
{jit_code_lei, "lei"},
{jit_code_ler_u, "ler_u"},
{jit_code_lei_u, "lei_u"},
{jit_code_eqr, "eqr"},
{jit_code_eqi, "eqi"},
{jit_code_ger, "ger"},
{jit_code_gei, "gei"},
{jit_code_ger_u, "ger_u"},
{jit_code_gei_u, "gei_u"},
{jit_code_gtr, "gtr"},
{jit_code_gti, "gti"},
{jit_code_gtr_u, "gtr_u"},
{jit_code_gti_u, "gti_u"},
{jit_code_ner, "ner"},
{jit_code_nei, "nei"},
{jit_code_movr, "movr"},
{jit_code_movi, "movi"},
{jit_code_extr_c, "extr_c"},
{jit_code_extr_uc, "extr_uc"},
{jit_code_extr_s, "extr_s"},
{jit_code_extr_us, "extr_us"},
{jit_code_extr_i, "extr_i"},
{jit_code_extr_ui, "extr_ui"},
{jit_code_htonr, "htonr"},
{jit_code_ldr_c, "ldr_c"},
{jit_code_ldi_c, "ldi_c"},
{jit_code_ldr_uc, "ldr_uc"},
{jit_code_ldi_uc, "ldi_uc"},
{jit_code_ldr_s, "ldr_s"},
{jit_code_ldi_s, "ldi_s"},
{jit_code_ldr_us, "ldr_us"},
{jit_code_ldi_us, "ldi_us"},
{jit_code_ldr_i, "ldr_i"},
{jit_code_ldi_i, "ldi_i"},
{jit_code_ldr_ui, "ldr_ui"},
{jit_code_ldi_ui, "ldi_ui"},
{jit_code_ldr_l, "ldr_l"},
{jit_code_ldi_l, "ldi_l"},
{jit_code_ldxr_c, "ldxr_c"},
{jit_code_ldxi_c, "ldxi_c"},
{jit_code_ldxr_uc, "ldxr_uc"},
{jit_code_ldxi_uc, "ldxi_uc"},
{jit_code_ldxr_s, "ldxr_s"},
{jit_code_ldxi_s, "ldxi_s"},
{jit_code_ldxr_us, "ldxr_us"},
{jit_code_ldxi_us, "ldxi_us"},
{jit_code_ldxr_i, "ldxr_i"},
{jit_code_ldxi_i, "ldxi_i"},
{jit_code_ldxr_ui, "ldxr_ui"},
{jit_code_ldxi_ui, "ldxi_ui"},
{jit_code_ldxr_l, "ldxr_l"},
{jit_code_ldxi_l, "ldxi_l"},
{jit_code_str_c, "str_c"},
{jit_code_sti_c, "sti_c"},
{jit_code_str_s, "str_s"},
{jit_code_sti_s, "sti_s"},
{jit_code_str_i, "str_i"},
{jit_code_sti_i, "sti_i"},
{jit_code_str_l, "str_l"},
{jit_code_sti_l, "sti_l"},
{jit_code_stxr_c, "stxr_c"},
{jit_code_stxi_c, "stxi_c"},
{jit_code_stxr_s, "stxr_s"},
{jit_code_stxi_s, "stxi_s"},
{jit_code_stxr_i, "stxr_i"},
{jit_code_stxi_i, "stxi_i"},
{jit_code_stxr_l, "stxr_l"},
{jit_code_stxi_l, "stxi_l"},
{jit_code_bltr, "bltr"},
{jit_code_blti, "blti"},
{jit_code_bltr_u, "bltr_u"},
{jit_code_blti_u, "blti_u"},
{jit_code_bler, "bler"},
{jit_code_blei, "blei"},
{jit_code_bler_u, "bler_u"},
{jit_code_blei_u, "blei_u"},
{jit_code_beqr, "beqr"},
{jit_code_beqi, "beqi"},
{jit_code_bger, "bger"},
{jit_code_bgei, "bgei"},
{jit_code_bger_u, "bger_u"},
{jit_code_bgei_u, "bgei_u"},
{jit_code_bgtr, "bgtr"},
{jit_code_bgti, "bgti"},
{jit_code_bgtr_u, "bgtr_u"},
{jit_code_bgti_u, "bgti_u"},
{jit_code_bner, "bner"},
{jit_code_bnei, "bnei"},
{jit_code_bmsr, "bmsr"},
{jit_code_bmsi, "bmsi"},
{jit_code_bmcr, "bmcr"},
{jit_code_bmci, "bmci"},
{jit_code_boaddr, "boaddr"},
{jit_code_boaddi, "boaddi"},
{jit_code_boaddr_u, "boaddr_u"},
{jit_code_boaddi_u, "boaddi_u"},
{jit_code_bxaddr, "bxaddr"},
{jit_code_bxaddi, "bxaddi"},
{jit_code_bxaddr_u, "bxaddr_u"},
{jit_code_bxaddi_u, "bxaddi_u"},
{jit_code_bosubr, "bosubr"},
{jit_code_bosubi, "bosubi"},
{jit_code_bosubr_u, "bosubr_u"},
{jit_code_bosubi_u, "bosubi_u"},
{jit_code_bxsubr, "bxsubr"},
{jit_code_bxsubi, "bxsubi"},
{jit_code_bxsubr_u, "bxsubr_u"},
{jit_code_bxsubi_u, "bxsubi_u"},
{jit_code_jmpr, "jmpr"},
{jit_code_jmpi, "jmpi"},
{jit_code_callr, "callr"},
{jit_code_calli, "calli"},
{jit_code_epilog, "epilog"},
{jit_code_arg_f, "arg_f"},
{jit_code_addr_f, "addr_f"},
{jit_code_addi_f, "addi_f"},
{jit_code_subr_f, "subr_f"},
{jit_code_subi_f, "subi_f"},
{jit_code_mulr_f, "mulr_f"},
{jit_code_muli_f, "muli_f"},
{jit_code_divr_f, "divr_f"},
{jit_code_divi_f, "divi_f"},
{jit_code_negr_f, "negr_f"},
{jit_code_absr_f, "absr_f"},
{jit_code_sqrtr_f, "sqrtr_f"},
{jit_code_ltr_f, "ltr_f"},
{jit_code_lti_f, "lti_f"},
{jit_code_ler_f, "ler_f"},
{jit_code_lei_f, "lei_f"},
{jit_code_eqr_f, "eqr_f"},
{jit_code_eqi_f, "eqi_f"},
{jit_code_ger_f, "ger_f"},
{jit_code_gei_f, "gei_f"},
{jit_code_gtr_f, "gtr_f"},
{jit_code_gti_f, "gti_f"},
{jit_code_ner_f, "ner_f"},
{jit_code_nei_f, "nei_f"},
{jit_code_unltr_f, "unltr_f"},
{jit_code_unlti_f, "unlti_f"},
{jit_code_unler_f, "unler_f"},
{jit_code_unlei_f, "unlei_f"},
{jit_code_uneqr_f, "uneqr_f"},
{jit_code_uneqi_f, "uneqi_f"},
{jit_code_unger_f, "unger_f"},
{jit_code_ungei_f, "ungei_f"},
{jit_code_ungtr_f, "ungtr_f"},
{jit_code_ungti_f, "ungti_f"},
{jit_code_ltgtr_f, "ltgtr_f"},
{jit_code_ltgti_f, "ltgti_f"},
{jit_code_ordr_f, "ordr_f"},
{jit_code_ordi_f, "ordi_f"},
{jit_code_unordr_f, "unordr_f"},
{jit_code_unordi_f, "unordi_f"},
{jit_code_truncr_f_i, "truncr_f_i"},
{jit_code_truncr_f_l, "truncr_f_l"},
{jit_code_extr_f, "extr_f"},
{jit_code_extr_d_f, "extr_d_f"},
{jit_code_movr_f, "movr_f"},
{jit_code_movi_f, "movi_f"},
{jit_code_ldr_f, "ldr_f"},
{jit_code_ldi_f, "ldi_f"},
{jit_code_ldxr_f, "ldxr_f"},
{jit_code_ldxi_f, "ldxi_f"},
{jit_code_str_f, "str_f"},
{jit_code_sti_f, "sti_f"},
{jit_code_stxr_f, "stxr_f"},
{jit_code_stxi_f, "stxi_f"},
{jit_code_bltr_f, "bltr_f"},
{jit_code_blti_f, "blti_f"},
{jit_code_bler_f, "bler_f"},
{jit_code_blei_f, "blei_f"},
{jit_code_beqr_f, "beqr_f"},
{jit_code_beqi_f, "beqi_f"},
{jit_code_bger_f, "bger_f"},
{jit_code_bgei_f, "bgei_f"},
{jit_code_bgtr_f, "bgtr_f"},
{jit_code_bgti_f, "bgti_f"},
{jit_code_bner_f, "bner_f"},
{jit_code_bnei_f, "bnei_f"},
{jit_code_bunltr_f, "bunltr_f"},
{jit_code_bunlti_f, "bunlti_f"},
{jit_code_bunler_f, "bunler_f"},
{jit_code_bunlei_f, "bunlei_f"},
{jit_code_buneqr_f, "buneqr_f"},
{jit_code_buneqi_f, "buneqi_f"},
{jit_code_bunger_f, "bunger_f"},
{jit_code_bungei_f, "bungei_f"},
{jit_code_bungtr_f, "bungtr_f"},
{jit_code_bungti_f, "bungti_f"},
{jit_code_bltgtr_f, "bltgtr_f"},
{jit_code_bltgti_f, "bltgti_f"},
{jit_code_bordr_f, "bordr_f"},
{jit_code_bordi_f, "bordi_f"},
{jit_code_bunordr_f, "bunordr_f"},
{jit_code_bunordi_f, "bunordi_f"},
{jit_code_arg_d, "arg_d"},
{jit_code_addr_d, "addr_d"},
{jit_code_addi_d, "addi_d"},
{jit_code_subr_d, "subr_d"},
{jit_code_subi_d, "subi_d"},
{jit_code_mulr_d, "mulr_d"},
{jit_code_muli_d, "muli_d"},
{jit_code_divr_d, "divr_d"},
{jit_code_divi_d, "divi_d"},
{jit_code_negr_d, "negr_d"},
{jit_code_absr_d, "absr_d"},
{jit_code_sqrtr_d, "sqrtr_d"},
{jit_code_ltr_d, "ltr_d"},
{jit_code_lti_d, "lti_d"},
{jit_code_ler_d, "ler_d"},
{jit_code_lei_d, "lei_d"},
{jit_code_eqr_d, "eqr_d"},
{jit_code_eqi_d, "eqi_d"},
{jit_code_ger_d, "ger_d"},
{jit_code_gei_d, "gei_d"},
{jit_code_gtr_d, "gtr_d"},
{jit_code_gti_d, "gti_d"},
{jit_code_ner_d, "ner_d"},
{jit_code_nei_d, "nei_d"},
{jit_code_unltr_d, "unltr_d"},
{jit_code_unlti_d, "unlti_d"},
{jit_code_unler_d, "unler_d"},
{jit_code_unlei_d, "unlei_d"},
{jit_code_uneqr_d, "uneqr_d"},
{jit_code_uneqi_d, "uneqi_d"},
{jit_code_unger_d, "unger_d"},
{jit_code_ungei_d, "ungei_d"},
{jit_code_ungtr_d, "ungtr_d"},
{jit_code_ungti_d, "ungti_d"},
{jit_code_ltgtr_d, "ltgtr_d"},
{jit_code_ltgti_d, "ltgti_d"},
{jit_code_ordr_d, "ordr_d"},
{jit_code_ordi_d, "ordi_d"},
{jit_code_unordr_d, "unordr_d"},
{jit_code_unordi_d, "unordi_d"},
{jit_code_truncr_d_i, "truncr_d_i"},
{jit_code_truncr_d_l, "truncr_d_l"},
{jit_code_extr_d, "extr_d"},
{jit_code_extr_f_d, "extr_f_d"},
{jit_code_movr_d, "movr_d"},
{jit_code_movi_d, "movi_d"},
{jit_code_ldr_d, "ldr_d"},
{jit_code_ldi_d, "ldi_d"},
{jit_code_ldxr_d, "ldxr_d"},
{jit_code_ldxi_d, "ldxi_d"},
{jit_code_str_d, "str_d"},
{jit_code_sti_d, "sti_d"},
{jit_code_stxr_d, "stxr_d"},
{jit_code_stxi_d, "stxi_d"},
{jit_code_bltr_d, "bltr_d"},
{jit_code_blti_d, "blti_d"},
{jit_code_bler_d, "bler_d"},
{jit_code_blei_d, "blei_d"},
{jit_code_beqr_d, "beqr_d"},
{jit_code_beqi_d, "beqi_d"},
{jit_code_bger_d, "bger_d"},
{jit_code_bgei_d, "bgei_d"},
{jit_code_bgtr_d, "bgtr_d"},
{jit_code_bgti_d, "bgti_d"},
{jit_code_bner_d, "bner_d"},
{jit_code_bnei_d, "bnei_d"},
{jit_code_bunltr_d, "bunltr_d"},
{jit_code_bunlti_d, "bunlti_d"},
{jit_code_bunler_d, "bunler_d"},
{jit_code_bunlei_d, "bunlei_d"},
{jit_code_buneqr_d, "buneqr_d"},
{jit_code_buneqi_d, "buneqi_d"},
{jit_code_bunger_d, "bunger_d"},
{jit_code_bungei_d, "bungei_d"},
{jit_code_bungtr_d, "bungtr_d"},
{jit_code_bungti_d, "bungti_d"},
{jit_code_bltgtr_d, "bltgtr_d"},
{jit_code_bltgti_d, "bltgti_d"},
{jit_code_bordr_d, "bordr_d"},
{jit_code_bordi_d, "bordi_d"},
{jit_code_bunordr_d, "bunordr_d"},
{jit_code_bunordi_d, "bunordi_d"},
{jit_code_movr_w_f, "movr_w_f"},
{jit_code_movr_ww_d, "movr_ww_d"},
{jit_code_movr_w_d, "movr_w_d"},
{jit_code_movr_f_w, "movr_f_w"},
{jit_code_movi_f_w, "movi_f_w"},
{jit_code_movr_d_ww, "movr_d_ww"},
{jit_code_movi_d_ww, "movi_d_ww"},
{jit_code_movr_d_w, "movr_d_w"},
{jit_code_movi_d_w, "movi_d_w"},
{jit_code_x86_retval_f, "x86_retval_f"},
{jit_code_x86_retval_d, "x86_retval_d"}};

static int codetablelens = (int) (sizeof(codetable)) / (sizeof(struct codestrpair_));

static int codetablelen = (int) jit_code_x86_retval_d + 1;

static void debug_dump(void)
{
  int i=0;
  init_constants();
  while (i < constantslens) {
    printf("%3.3d %3.3d %s.\n",i, constants[i].number, constants[i].name);
    i++;
  }
  i = 0;
  while (i < codetablelen) {
    printf("%3.3d %3.3d %s.\n",i, codetable[i].code, codetable[i].name);
    i++;
  }
  i = 0;
  while (i < sizeslens) {
    printf("%3.3d %3.3d %s.\n",i, sizes[i].number, sizes[i].name);
    i++;
  }
  printf("Codetable is %d(%d) pairs. Lightning arch = %s, Word Size = %d, Byte Order = %s\n",
	 codetablelen,codetablelens, arch, wordsize, 
         byte_order == little_endian ? "littleendian" : "bigendian");

  printf("mosml_ffi_alloc is %p. mosml_ffi_free is %p. \n", &mosml_ffi_alloc, &mosml_ffi_free);
  return;
}

long jit_ffi_debug = 0;

value jit_initialise_constants(value debug)
{
  jit_ffi_debug = (Bool_val(debug)) ? 1 : 0;
  init_constants();

  if (jit_ffi_debug)
     debug_dump();

  return Val_unit;
}

value jit_set_memfuns(value unit)
{
  /*  jit_set_memory_functions((jit_alloc_func_ptr)&mosml_ffi_alloc,
                           (jit_realloc_func_ptr)&mosml_ffi_resize,
                           (jit_free_func_ptr)&mosml_ffi_free);

  jit_set_memory_functions((jit_alloc_func_ptr)&malloc,
                           (jit_realloc_func_ptr)&realloc,
                           (jit_free_func_ptr)&free); */

  jit_set_memory_functions((jit_alloc_func_ptr)&stat_alloc,
                           (jit_realloc_func_ptr)&stat_resize,
                           (jit_free_func_ptr)&stat_free);
  return Val_unit;
}

value jit_get_debug(value unit)
{
  return (Val_bool(jit_ffi_debug));
}

value jit_set_debug(value debug)
{
  jit_ffi_debug = (Bool_val(debug)) ? 1 : 0;
  return Val_unit;
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

value jit_get_codetablelength(value unit)
{
  return Val_long(codetablelens);
}

value jit_get_sizeslength(value unit)
{
  return Val_long(sizeslens);
}

value jit_get_constantslength(value unit)
{
  return Val_long(constantslens);
}

value jit_get_codetableentry(value num)
{   int entry = Long_val(num);

    if (entry >= 0 && entry < codetablelens)
      return copy_int_str_pair((intstrpair_t *)&codetable[entry]);
    return copy_int_str_pair((intstrpair_t *)&codetable[0]);
}

value jit_get_sizesentry(value num)
{   int entry = Long_val(num);

    if (entry >= 0 && entry < sizeslens)
      return copy_int_str_pair(&sizes[entry]);
    
    return copy_int_str_pair(&sizes[0]);

}

value jit_get_constantsentry(value num)
{   int entry = Long_val(num);

    if (entry >= 0 && entry < constantslens)
      return copy_int_str_pair(&constants[entry]);
    return copy_int_str_pair(&constants[0]);
}
