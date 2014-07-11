
datatype jit_gpr_t = 
   R0 | R1 | R2 | V0 | V1 | V2 | R of int | V of int

datatype jit_fpr_t = 
   F0 | F1 | F2 | F3 | F4 | F5 | F of int

datatype jit_sign =
   Unsigned
 | Signed

datatype jit_typed_value =
   State of Word8Vector.vector
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
val finish_jit : unit -> unit
val jit_new_state : unit -> jit_typed_value
val jit_clear_state : jit_typed_value -> unit
val jit_destroy_state : jit_typed_value -> unit
val jit_label : jit_typed_value -> jit_typed_value
val jit_arg : jit_typed_value -> jit_typed_value
val jit_prolog : jit_typed_value -> unit
val jit_prepare : jit_typed_value -> unit
val jit_disassemble : jit_typed_value -> unit
val jit_pushargr : jit_typed_value * jit_gpr_t -> unit
val jit_finishi : jit_typed_value * jit_typed_value -> jit_typed_value
val jit_getarg : jit_typed_value * jit_gpr_t * jit_typed_value -> unit
val jit_retval : jit_typed_value * jit_gpr_t -> unit
val jit_patch : jit_typed_value * jit_typed_value -> unit
val jit_patch_at : jit_typed_value * jit_typed_value * jit_typed_value -> unit
val jit_retr : jit_typed_value * jit_gpr_t -> unit
val jit_new_node_pww : jit_typed_value * jit_typed_value * jit_typed_value * Word.word * Word.word -> jit_typed_value
val jit_blti : jit_typed_value * jit_gpr_t * int -> jit_typed_value
val jit_new_node_www : jit_typed_value * jit_typed_value * Word.word * Word.word * Word.word -> jit_typed_value
val jit_subi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_addi : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> jit_typed_value
val jit_subr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_addr : jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> jit_typed_value
val jit_new_node_ww : jit_typed_value * jit_typed_value * Word.word * Word.word -> jit_typed_value
val jit_emit : jit_typed_value -> Dynlib.cptr
val jit_movi : jit_typed_value * jit_gpr_t * int -> jit_typed_value
