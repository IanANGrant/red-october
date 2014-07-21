signature WordOps = sig
   type state
   type gpr
   val jit_addr : state * gpr * gpr * gpr -> state
   val jit_subr : state * gpr * gpr * gpr -> state
   val jit_mulr : state * gpr * gpr * gpr -> state
   val jit_divr_u : state * gpr * gpr * gpr -> state
   val jit_remr_u : state * gpr * gpr * gpr -> state
   val jit_rshr : state * gpr * gpr * gpr -> state
   val jit_rshr_u : state * gpr * gpr * gpr -> state
   val jit_lshr : state * gpr * gpr * gpr -> state
   val jit_andr : state * gpr * gpr * gpr -> state
   val jit_orr : state * gpr * gpr * gpr -> state
   val jit_xorr : state * gpr * gpr * gpr -> state
   val jit_comr : state * gpr * gpr -> state
   val jit_negr : state * gpr * gpr -> state
   val jit_eqr : state * gpr * gpr * gpr -> state
   val jit_ltr_u : state * gpr * gpr * gpr -> state
   val jit_ler_u : state * gpr * gpr * gpr -> state
   val jit_gtr_u : state * gpr * gpr * gpr -> state
   val jit_ger_u : state * gpr * gpr * gpr -> state
end
