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
