structure WordOps
    :> WordOps where type state = Jit.jit_typed_value
                 and type gpr = Jit.jit_gpr_t =
 struct
   type state = Jit.jit_typed_value
   type gpr = Jit.jit_gpr_t
   val jit_addr :   state * gpr * gpr * gpr -> state = Jit.jit_addr
   val jit_subr :   state * gpr * gpr * gpr -> state = Jit.jit_subr
   val jit_mulr :   state * gpr * gpr * gpr -> state = Jit.jit_mulr
   val jit_divr_u : state * gpr * gpr * gpr -> state = Jit.jit_divr_u
   val jit_remr_u : state * gpr * gpr * gpr -> state = Jit.jit_remr_u
   val jit_rshr :   state * gpr * gpr * gpr -> state = Jit.jit_rshr
   val jit_rshr_u : state * gpr * gpr * gpr -> state = Jit.jit_rshr_u
   val jit_lshr :   state * gpr * gpr * gpr -> state = Jit.jit_lshr
   val jit_andr :   state * gpr * gpr * gpr -> state = Jit.jit_andr
   val jit_orr :    state * gpr * gpr * gpr -> state = Jit.jit_orr
   val jit_xorr :   state * gpr * gpr * gpr -> state = Jit.jit_xorr
   val jit_eqr :    state * gpr * gpr * gpr -> state = Jit.jit_eqr
   val jit_ltr_u :  state * gpr * gpr * gpr -> state = Jit.jit_ltr_u
   val jit_ler_u :  state * gpr * gpr * gpr -> state = Jit.jit_ler_u
   val jit_gtr_u :  state * gpr * gpr * gpr -> state = Jit.jit_gtr_u
   val jit_ger_u :  state * gpr * gpr * gpr -> state = Jit.jit_ger_u
   val jit_comr :   state * gpr * gpr -> state = Jit.jit_comr
   val jit_negr :   state * gpr * gpr -> state = Jit.jit_negr
end
