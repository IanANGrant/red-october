
datatype jit_gpr_t = 
   R0 | R1 | R2 | V0 | V1 | V2 | FP | R of int | V of int

datatype jit_fpr_t = 
   F0 | F1 | F2 | F3 | F4 | F5 | F of int

datatype jit_sign =
   Unsigned
 | Signed

datatype jit_typed_value =
   State of Word8Vector.vector ref
 | Node of Word8Vector.vector
 | String of Word8Vector.vector
 | Code of Word8Vector.vector
 | GPRegister of Word8Vector.vector
 | FPRegister of Word8Vector.vector
 | Label of Word8Vector.vector
 | Character of Word8.word * jit_sign
 | Short of Word8Vector.vector * jit_sign
 | Integer of Word8Vector.vector * jit_sign
 | Long of Word8Vector.vector * jit_sign
 | Float of Word8Vector.vector
 | Double of Word8Vector.vector
 | Boolean of Word8.word
 | Pointer of Word8Vector.vector
 | Structure of Word8Vector.vector list

val argv0svec : Ffi.svector
val argv0 : jit_typed_value
val NULLp : jit_typed_value
val jit_r : int -> Word.word
val jit_v : int -> Word.word
val R_NUM : int
val V_NUM : int
val MAX_R : int
val MAX_V : int
val R3 : jit_gpr_t
val V3 : jit_gpr_t
val F6 : jit_fpr_t
val F7 : jit_fpr_t
val jit_gpr : jit_gpr_t -> Word.word

val WORDSIZE : int
val BYTE_ORDER : int
val LITTLE_ENDIAN : int
val BIG_ENDIAN : int

val init_jit : jit_typed_value -> unit
val jit_set_memory_functions : Dynlib.cptr -> Dynlib.cptr -> Dynlib.cptr -> unit
val finish_jit : unit -> unit
val jit_new_state : unit -> jit_typed_value
val jit_clear_state : jit_typed_value -> unit
val jit_destroy_state : jit_typed_value -> unit
val jit_label : jit_typed_value -> jit_typed_value
val jit_arg : jit_typed_value -> jit_typed_value
val jit_prolog : jit_typed_value -> unit
val jit_epilog : jit_typed_value -> unit
val jit_prepare : jit_typed_value -> unit
val jit_disassemble : jit_typed_value -> unit
val jit_realize : jit_typed_value -> unit
val jit_emit : jit_typed_value -> Dynlib.cptr
val jit_pushargr : jit_typed_value * jit_gpr_t -> unit
val jit_allocai : jit_typed_value * int -> int
val jit_pushargi : jit_typed_value * int -> unit
val jit_pushargi_u : jit_typed_value * word -> unit
val jit_finishi : jit_typed_value * jit_typed_value -> jit_typed_value
val jit_finishr : jit_typed_value * jit_gpr_t -> jit_typed_value
val jit_getarg : jit_typed_value * jit_gpr_t * jit_typed_value -> unit
val jit_getarg_c : jit_typed_value * jit_gpr_t * jit_typed_value -> unit
val jit_getarg_uc : jit_typed_value * jit_gpr_t * jit_typed_value -> unit
val jit_getarg_s : jit_typed_value * jit_gpr_t * jit_typed_value -> unit
val jit_getarg_us : jit_typed_value * jit_gpr_t * jit_typed_value -> unit
val jit_getarg_i : jit_typed_value * jit_gpr_t * jit_typed_value -> unit
val jit_getarg_ui : jit_typed_value * jit_gpr_t * jit_typed_value -> unit
val jit_getarg_l : jit_typed_value * jit_gpr_t * jit_typed_value -> unit
val jit_retval : jit_typed_value * jit_gpr_t -> unit
val jit_retval_c : jit_typed_value * jit_gpr_t -> unit
val jit_retval_uc : jit_typed_value * jit_gpr_t -> unit
val jit_retval_s : jit_typed_value * jit_gpr_t -> unit
val jit_retval_us : jit_typed_value * jit_gpr_t -> unit
val jit_retval_i : jit_typed_value * jit_gpr_t -> unit
val jit_retval_ui : jit_typed_value * jit_gpr_t -> unit
val jit_retval_l : jit_typed_value * jit_gpr_t -> unit
val jit_patch : jit_typed_value * jit_typed_value -> unit
val jit_address : jit_typed_value * jit_typed_value -> Dynlib.cptr
val jit_patch_at : jit_typed_value * jit_typed_value * jit_typed_value -> unit
val jit_ret : jit_typed_value -> unit
val jit_retr : jit_typed_value * jit_gpr_t -> unit
val jit_reti : jit_typed_value * word -> unit
val jit_new_node_pw : jit_typed_value * jit_typed_value * jit_typed_value * word -> jit_typed_value
val jit_new_node_wp : jit_typed_value * jit_typed_value * word * jit_typed_value -> jit_typed_value
val jit_new_node_pww : jit_typed_value * jit_typed_value * jit_typed_value * word * word -> jit_typed_value
val jit_bgti : jit_typed_value * jit_gpr_t * int -> jit_typed_value
val jit_bgti_u : jit_typed_value * jit_gpr_t * int -> jit_typed_value
val jit_bgei : jit_typed_value * jit_gpr_t * int -> jit_typed_value
val jit_bgei_u : jit_typed_value * jit_gpr_t * int -> jit_typed_value
val jit_blti : jit_typed_value * jit_gpr_t * int -> jit_typed_value
val jit_blti_u : jit_typed_value * jit_gpr_t * int -> jit_typed_value
val jit_blei : jit_typed_value * jit_gpr_t * int -> jit_typed_value
val jit_blei_u : jit_typed_value * jit_gpr_t * int -> jit_typed_value
val jit_beqi : jit_typed_value * jit_gpr_t * int -> jit_typed_value
val jit_bnei : jit_typed_value * jit_gpr_t * int -> jit_typed_value
val jit_bgtr : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_bgtr_u : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_bger : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_bger_u : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_bltr : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_bltr_u : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_bler : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_bler_u : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_beqr : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_bner : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_new_node_www : jit_typed_value * jit_typed_value * word * word * word -> jit_typed_value
val jit_new_node_wwp : jit_typed_value * jit_typed_value * word * word * jit_typed_value -> jit_typed_value
val jit_gti : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_gti_u : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_gei : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_gei_u : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_lti : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_lti_u : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_lei : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_lei_u : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_eqi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_nei : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_gtr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_gtr_u : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ger : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ger_u : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ltr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ltr_u : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ler : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ler_u : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_eqr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ner : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_subi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_addi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_subci : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_addci : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_subxi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_addxi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_muli : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_divi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_remi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_divi_u : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_remi_u : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_andi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_ori : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_xori : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_lshi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_rshi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_rshi_u : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_subr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_addr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_subcr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_addcr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_subxr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_addxr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_mulr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_divr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_remr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_divr_u : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_remr_u : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_andr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_orr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_xorr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_lshr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_rshr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_rshr_u : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_new_node_ww : jit_typed_value * jit_typed_value * word * word -> jit_typed_value
val jit_movi : jit_typed_value * jit_gpr_t * int -> jit_typed_value
val jit_movi_p : jit_typed_value * jit_gpr_t * jit_typed_value -> jit_typed_value
val jit_movr : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_negr : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_comr : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ntohr : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_htonr : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldr : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldi : jit_typed_value * jit_gpr_t * jit_typed_value -> jit_typed_value
val jit_ldr_c : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldi_c : jit_typed_value * jit_gpr_t * jit_typed_value -> jit_typed_value
val jit_ldr_uc : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldi_uc : jit_typed_value * jit_gpr_t * jit_typed_value -> jit_typed_value
val jit_ldr_s : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldi_s : jit_typed_value * jit_gpr_t * jit_typed_value -> jit_typed_value
val jit_ldr_us : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldi_us : jit_typed_value * jit_gpr_t * jit_typed_value -> jit_typed_value
val jit_ldr_i : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldi_i : jit_typed_value * jit_gpr_t * jit_typed_value -> jit_typed_value
val jit_ldr_ui : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldi_ui : jit_typed_value * jit_gpr_t * jit_typed_value -> jit_typed_value
val jit_ldr_l : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldi_l : jit_typed_value * jit_gpr_t * jit_typed_value -> jit_typed_value
val jit_ldxr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldxi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_ldxr_c : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldxi_c : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_ldxr_uc : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldxi_uc : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_ldxr_s : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldxi_s : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_ldxr_us : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldxi_us : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_ldxr_i : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldxi_i : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_ldxr_ui : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldxi_ui : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_ldxr_l : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_ldxi_l : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_str : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_sti : jit_typed_value * jit_typed_value * jit_gpr_t -> jit_typed_value
val jit_str_c : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_sti_c : jit_typed_value * jit_typed_value * jit_gpr_t -> jit_typed_value
val jit_str_s : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_sti_s : jit_typed_value * jit_typed_value * jit_gpr_t -> jit_typed_value
val jit_str_i : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_sti_i : jit_typed_value * jit_typed_value * jit_gpr_t -> jit_typed_value
val jit_str_l : jit_typed_value * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_sti_l : jit_typed_value * jit_typed_value * jit_gpr_t -> jit_typed_value
val jit_stxr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_stxi : jit_typed_value * int * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_stxr_c : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_stxi_c : jit_typed_value * int * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_stxr_s : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_stxi_s : jit_typed_value * int * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_stxr_i : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_stxi_i : jit_typed_value * int * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_stxr_l : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_stxi_l : jit_typed_value * int * jit_gpr_t * jit_gpr_t -> jit_typed_value
