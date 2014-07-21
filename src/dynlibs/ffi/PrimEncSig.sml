open Jit
signature PrimEnc =
sig
   type excn
   val exnDiv : excn
   val jit_atom : jit_typed_value * jit_gpr_t * jit_gpr_t -> unit
   val jit_binop_dnz : (jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> 'a) ->
                        jit_typed_value * jit_gpr_t * jit_gpr_t * jit_gpr_t -> 'a
   val jit_long_val : jit_typed_value * jit_gpr_t -> unit
   val jit_raise_exn : jit_typed_value * excn -> unit
   val jit_val_bool : jit_typed_value * jit_gpr_t * jit_gpr_t -> unit
   val jit_val_long : jit_typed_value * jit_gpr_t -> unit
   val jit_ldargl : jit_typed_value * jit_gpr_t * jit_gpr_t * int -> unit
end
