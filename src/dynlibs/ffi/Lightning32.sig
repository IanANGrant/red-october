type state
type node
type code

datatype jit_gpr_t = 
   R0 | R1 | R2 | V0 | V1 | V2 | FP | R of int | V of int

datatype jit_fpr_t = 
   F0 | F1 | F2 | F3 | F4 | F5 | F of int

val WORDSIZE : int
val BYTE_ORDER : int
val LITTLE_ENDIAN : int
val BIG_ENDIAN : int

val R_NUM : int
val V_NUM : int
val F_NUM : int

val MAX_R : int
val MAX_V : int
val MAX_F : int

val NULL : Dynlib.cptr

val init_jit : unit -> unit
val finish_jit : unit -> unit

val jit_new_state : unit -> state

val var  : Dynlib.cptr -> 'b
val app1 : Dynlib.cptr -> 'a1 -> 'b
val app2 : Dynlib.cptr -> 'a1 -> 'a2 -> 'b
val app3 : Dynlib.cptr -> 'a1 -> 'a2 -> 'a3 -> 'b
val app4 : Dynlib.cptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'b
val app5 : Dynlib.cptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b
val jit_address : state * node -> Dynlib.cptr
val jit_allocai : state * word -> int
val jit_arg : state -> node
val jit_arg_d : state -> node
val jit_arg_f : state -> node
val jit_clear_state : state -> unit
val jit_destroy_state : state -> unit
val jit_disassemble : state -> unit
val jit_ellipsis : state -> unit
val jit_emit : state -> Dynlib.cptr
val jit_epilog : state -> unit
val jit_finishi : state * Dynlib.cptr -> node
val jit_finishr : state * jit_gpr_t -> unit
val jit_forward : state -> unit
val jit_getarg_c : state * jit_gpr_t * node -> unit
val jit_getarg_d : state * jit_fpr_t * node -> unit
val jit_getarg_f : state * jit_fpr_t * node -> unit
val jit_getarg_i : state * jit_gpr_t * node -> unit
val jit_getarg : state * jit_gpr_t * node -> unit
val jit_getarg_s : state * jit_gpr_t * node -> unit
val jit_getarg_uc : state * jit_gpr_t * node -> unit
val jit_getarg_us : state * jit_gpr_t * node -> unit
val jit_indirect : state -> node
val jit_label : state -> unit
val jit_new_node : state * code -> node
val jit_note : state * Dynlib.cptr * word -> node
val jit_patch : state * node -> unit
val jit_patch_abs : state * node * Dynlib.cptr -> unit
val jit_patch_at : state * node * node -> unit
val jit_prepare : state -> unit
val jit_print : state -> unit
val jit_prolog : state -> unit
val jit_pushargi : state * word -> unit
val jit_pushargi_d : state * real -> unit
val jit_pushargi_f : state * real -> unit
val jit_pushargr : state * jit_gpr_t -> unit
val jit_pushargr_d : state * jit_fpr_t -> unit
val jit_pushargr_f : state * jit_fpr_t -> unit
val jit_realize : state -> unit
val jit_ret : state -> unit
val jit_reti : state * word -> unit
val jit_reti_d : state * real -> unit
val jit_reti_f : state * real -> unit
val jit_retr : state * jit_gpr_t -> unit
val jit_retr_d : state * jit_fpr_t -> unit
val jit_retr_f : state * jit_fpr_t -> unit
val jit_retval_c : state * jit_gpr_t -> unit
val jit_retval_d : state * jit_fpr_t -> unit
val jit_retval_f : state * jit_fpr_t -> unit
val jit_retval_i : state * jit_gpr_t -> unit
val jit_retval : state * jit_gpr_t -> unit
val jit_retval_s : state * jit_gpr_t -> unit
val jit_retval_uc : state * jit_gpr_t -> unit
val jit_retval_us : state * jit_gpr_t -> unit
val jit_absr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_absr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_addci : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_addcr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_addi : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_addi_d : state * jit_fpr_t * jit_fpr_t * real -> node
val jit_addi_f : state * jit_fpr_t * jit_fpr_t * real -> node
val jit_addr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_addr_d : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_addr_f : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_addxi : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_addxr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_andi : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_andr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_beqi : state * jit_gpr_t * word -> node
val jit_beqi_d : state * jit_fpr_t * real -> node
val jit_beqi_f : state * jit_fpr_t * real -> node
val jit_beqr : state * jit_gpr_t * jit_gpr_t -> node
val jit_beqr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_beqr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_bgei : state * jit_gpr_t * word -> node
val jit_bgei_d : state * jit_fpr_t * real -> node
val jit_bgei_f : state * jit_fpr_t * real -> node
val jit_bgei_u : state * jit_gpr_t * word -> node
val jit_bger : state * jit_gpr_t * jit_gpr_t -> node
val jit_bger_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_bger_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_bger_u : state * jit_gpr_t * jit_gpr_t -> node
val jit_bgti : state * jit_gpr_t * word -> node
val jit_bgti_d : state * jit_fpr_t * real -> node
val jit_bgti_f : state * jit_fpr_t * real -> node
val jit_bgti_u : state * jit_gpr_t * word -> node
val jit_bgtr : state * jit_gpr_t * jit_gpr_t -> node
val jit_bgtr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_bgtr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_bgtr_u : state * jit_gpr_t * jit_gpr_t -> node
val jit_blei : state * jit_gpr_t * word -> node
val jit_blei_d : state * jit_fpr_t * real -> node
val jit_blei_f : state * jit_fpr_t * real -> node
val jit_blei_u : state * jit_gpr_t * word -> node
val jit_bler : state * jit_gpr_t * jit_gpr_t -> node
val jit_bler_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_bler_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_bler_u : state * jit_gpr_t * jit_gpr_t -> node
val jit_bltgti_d : state * jit_fpr_t * real -> node
val jit_bltgti_f : state * jit_fpr_t * real -> node
val jit_bltgtr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_bltgtr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_blti : state * jit_gpr_t * word -> node
val jit_blti_d : state * jit_fpr_t * real -> node
val jit_blti_f : state * jit_fpr_t * real -> node
val jit_blti_u : state * jit_gpr_t * word -> node
val jit_bltr : state * jit_gpr_t * jit_gpr_t -> node
val jit_bltr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_bltr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_bltr_u : state * jit_gpr_t * jit_gpr_t -> node
val jit_bmci : state * jit_gpr_t * word -> node
val jit_bmcr : state * jit_gpr_t * jit_gpr_t -> node
val jit_bmsi : state * jit_gpr_t * word -> node
val jit_bmsr : state * jit_gpr_t * jit_gpr_t -> node
val jit_bnei : state * jit_gpr_t * word -> node
val jit_bnei_d : state * jit_fpr_t * real -> node
val jit_bnei_f : state * jit_fpr_t * real -> node
val jit_bner : state * jit_gpr_t * jit_gpr_t -> node
val jit_bner_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_bner_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_boaddi : state * jit_gpr_t * word -> node
val jit_boaddi_u : state * jit_gpr_t * word -> node
val jit_boaddr : state * jit_gpr_t * jit_gpr_t -> node
val jit_boaddr_u : state * jit_gpr_t * jit_gpr_t -> node
val jit_bordi_d : state * jit_fpr_t * real -> node
val jit_bordi_f : state * jit_fpr_t * real -> node
val jit_bordr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_bordr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_bosubi : state * jit_gpr_t * word -> node
val jit_bosubi_u : state * jit_gpr_t * word -> node
val jit_bosubr : state * jit_gpr_t * jit_gpr_t -> node
val jit_bosubr_u : state * jit_gpr_t * jit_gpr_t -> node
val jit_buneqi_d : state * jit_fpr_t * real -> node
val jit_buneqi_f : state * jit_fpr_t * real -> node
val jit_buneqr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_buneqr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_bungei_d : state * jit_fpr_t * real -> node
val jit_bungei_f : state * jit_fpr_t * real -> node
val jit_bunger_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_bunger_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_bungti_d : state * jit_fpr_t * real -> node
val jit_bungti_f : state * jit_fpr_t * real -> node
val jit_bungtr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_bungtr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_bunlei_d : state * jit_fpr_t * real -> node
val jit_bunlei_f : state * jit_fpr_t * real -> node
val jit_bunler_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_bunler_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_bunlti_d : state * jit_fpr_t * real -> node
val jit_bunlti_f : state * jit_fpr_t * real -> node
val jit_bunltr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_bunltr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_bunordi_d : state * jit_fpr_t * real -> node
val jit_bunordi_f : state * jit_fpr_t * real -> node
val jit_bunordr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_bunordr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_bxaddi : state * jit_gpr_t * word -> node
val jit_bxaddi_u : state * jit_gpr_t * word -> node
val jit_bxaddr : state * jit_gpr_t * jit_gpr_t -> node
val jit_bxaddr_u : state * jit_gpr_t * jit_gpr_t -> node
val jit_bxsubi : state * jit_gpr_t * word -> node
val jit_bxsubi_u : state * jit_gpr_t * word -> node
val jit_bxsubr : state * jit_gpr_t * jit_gpr_t -> node
val jit_bxsubr_u : state * jit_gpr_t * jit_gpr_t -> node
val jit_calli : state * Dynlib.cptr -> node
val jit_callr : state * jit_gpr_t -> node
val jit_comr : state * jit_gpr_t * jit_gpr_t -> node
val jit_divi : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_divi_d : state * jit_fpr_t * jit_fpr_t * real -> node
val jit_divi_f : state * jit_fpr_t * jit_fpr_t * real -> node
val jit_divi_u : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_divr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_divr_d : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_divr_f : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_divr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_eqi : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_eqi_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_eqi_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_eqr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_eqr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_eqr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_extr_c : state * jit_gpr_t * jit_fpr_t -> node
val jit_extr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_extr_d_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_extr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_extr_f_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_extr_s : state * jit_gpr_t * jit_fpr_t -> node
val jit_extr_uc : state * jit_gpr_t * jit_fpr_t -> node
val jit_extr_us : state * jit_gpr_t * jit_fpr_t -> node
val jit_gei : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_gei_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_gei_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_gei_u : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_ger : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_ger_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ger_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ger_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_gti : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_gti_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_gti_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_gti_u : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_gtr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_gtr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_gtr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_gtr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_htonr : state * jit_gpr_t * jit_gpr_t -> node
val jit_jmpi : state -> node
val jit_jmpr : state * jit_gpr_t -> node
val jit_ldi_c : state * jit_gpr_t * Dynlib.cptr -> node
val jit_ldi_d : state * jit_fpr_t * Dynlib.cptr -> node
val jit_ldi_f : state * jit_fpr_t * Dynlib.cptr -> node
val jit_ldi_i : state * jit_gpr_t * Dynlib.cptr -> node
val jit_ldi : state * jit_gpr_t * Dynlib.cptr -> node
val jit_ldi_s : state * jit_gpr_t * Dynlib.cptr -> node
val jit_ldi_uc : state * jit_gpr_t * Dynlib.cptr -> node
val jit_ldi_us : state * jit_gpr_t * Dynlib.cptr -> node
val jit_ldr_c : state * jit_gpr_t * jit_gpr_t -> node
val jit_ldr_d : state * jit_fpr_t * jit_gpr_t -> node
val jit_ldr_f : state * jit_fpr_t * jit_gpr_t -> node
val jit_ldr_i : state * jit_gpr_t * jit_gpr_t -> node
val jit_ldr : state * jit_gpr_t * jit_gpr_t -> node
val jit_ldr_s : state * jit_gpr_t * jit_gpr_t -> node
val jit_ldr_uc : state * jit_gpr_t * jit_gpr_t -> node
val jit_ldr_us : state * jit_gpr_t * jit_gpr_t -> node
val jit_ldxi_c : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_ldxi_d : state * jit_fpr_t * jit_gpr_t * word -> node
val jit_ldxi_f : state * jit_fpr_t * jit_gpr_t * word -> node
val jit_ldxi_i : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_ldxi : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_ldxi_s : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_ldxi_uc : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_ldxi_us : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_ldxr_c : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_ldxr_d : state * jit_fpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_ldxr_f : state * jit_fpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_ldxr_i : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_ldxr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_ldxr_s : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_ldxr_uc : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_ldxr_us : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_lei : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_lei_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_lei_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_lei_u : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_ler : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_ler_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ler_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ler_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_live : state * jit_gpr_t -> node
val jit_lshi : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_lshr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_ltgti_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_ltgti_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_ltgtr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ltgtr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_lti : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_lti_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_lti_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_lti_u : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_ltr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_ltr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ltr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ltr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_movi : state * jit_gpr_t * word -> node
val jit_movi_d : state * jit_fpr_t * real -> node
val jit_movi_d_w : state * jit_gpr_t * real -> node
val jit_movi_d_ww : state * jit_gpr_t * jit_gpr_t * real -> node
val jit_movi_f : state * jit_fpr_t * real -> node
val jit_movi_f_w : state * jit_gpr_t * real -> node
val jit_movr : state * jit_gpr_t * jit_gpr_t -> node
val jit_movr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_movr_d_w : state * jit_gpr_t * jit_fpr_t -> node
val jit_movr_d_ww : state * jit_gpr_t * jit_gpr_t * jit_fpr_t -> node
val jit_movr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_movr_f_w : state * jit_gpr_t * jit_fpr_t -> node
val jit_movr_w_d : state * jit_fpr_t * jit_gpr_t -> node
val jit_movr_w_f : state * jit_fpr_t * jit_gpr_t -> node
val jit_movr_ww_d : state * jit_fpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_muli : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_muli_d : state * jit_fpr_t * jit_fpr_t * real -> node
val jit_muli_f : state * jit_fpr_t * jit_fpr_t * real -> node
val jit_mulr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_mulr_d : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_mulr_f : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_negr : state * jit_gpr_t * jit_gpr_t -> node
val jit_negr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_negr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_nei : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_nei_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_nei_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_ner : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_ner_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ner_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ntohr : state * jit_gpr_t * jit_gpr_t -> node
val jit_ordi_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_ordi_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_ordr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ordr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ori : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_orr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_qdivi : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * word -> node
val jit_qdivi_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * word -> node
val jit_qdivr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_qdivr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_qmuli : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * word -> node
val jit_qmuli_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * word -> node
val jit_qmulr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_qmulr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_remi : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_remi_u : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_remr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_remr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_rshi : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_rshi_u : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_rshr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_rshr_u : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_sqrtr_d : state * jit_fpr_t * jit_fpr_t -> node
val jit_sqrtr_f : state * jit_fpr_t * jit_fpr_t -> node
val jit_sti_c : state * Dynlib.cptr * jit_gpr_t -> node
val jit_sti_d : state * Dynlib.cptr * jit_fpr_t -> node
val jit_sti_f : state * Dynlib.cptr * jit_fpr_t -> node
val jit_sti_i : state * Dynlib.cptr * jit_gpr_t -> node
val jit_sti : state * Dynlib.cptr * jit_gpr_t -> node
val jit_sti_s : state * Dynlib.cptr * jit_gpr_t -> node
val jit_str_c : state * jit_gpr_t * jit_gpr_t -> node
val jit_str_d : state * jit_gpr_t * jit_fpr_t -> node
val jit_str_f : state * jit_gpr_t * jit_fpr_t -> node
val jit_str_i : state * jit_gpr_t * jit_gpr_t -> node
val jit_str : state * jit_gpr_t * jit_gpr_t -> node
val jit_str_s : state * jit_gpr_t * jit_gpr_t -> node
val jit_stxi_c : state * word * jit_gpr_t * jit_gpr_t -> node
val jit_stxi_d : state * word * jit_gpr_t * jit_fpr_t -> node
val jit_stxi_f : state * word * jit_gpr_t * jit_fpr_t -> node
val jit_stxi_i : state * word * jit_gpr_t * jit_gpr_t -> node
val jit_stxi : state * word * jit_gpr_t * jit_gpr_t -> node
val jit_stxi_s : state * word * jit_gpr_t * jit_gpr_t -> node
val jit_stxr_c : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_stxr_d : state * jit_gpr_t * jit_gpr_t * jit_fpr_t -> node
val jit_stxr_f : state * jit_gpr_t * jit_gpr_t * jit_fpr_t -> node
val jit_stxr_i : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_stxr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_stxr_s : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_subci : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_subcr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_subi : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_subi_d : state * jit_fpr_t * jit_fpr_t * real -> node
val jit_subi_f : state * jit_fpr_t * jit_fpr_t * real -> node
val jit_subr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_subr_d : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_subr_f : state * jit_fpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_subxi : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_subxr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
val jit_truncr_d_i : state * jit_gpr_t * jit_fpr_t -> node
val jit_truncr_d : state * jit_gpr_t * jit_fpr_t -> node
val jit_truncr_f_i : state * jit_gpr_t * jit_fpr_t -> node
val jit_truncr_f : state * jit_gpr_t * jit_fpr_t -> node
val jit_uneqi_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_uneqi_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_uneqr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_uneqr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ungei_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_ungei_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_unger_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_unger_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ungti_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_ungti_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_ungtr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_ungtr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_unlei_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_unlei_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_unler_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_unler_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_unlti_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_unlti_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_unltr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_unltr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_unordi_d : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_unordi_f : state * jit_gpr_t * jit_fpr_t * real -> node
val jit_unordr_d : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_unordr_f : state * jit_gpr_t * jit_fpr_t * jit_fpr_t -> node
val jit_xori : state * jit_gpr_t * jit_gpr_t * word -> node
val jit_xorr : state * jit_gpr_t * jit_gpr_t * jit_gpr_t -> node
