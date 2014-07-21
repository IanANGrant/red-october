signature WordEnc = 
sig
   eqtype word
   type largeword
   type excn
   val exnDiv : excn
   val wordSize : int
   val mkVal : unit -> word
   val jit_atom : Jit.jit_typed_value * Jit.jit_gpr_t * Jit.jit_gpr_t -> unit
   val jit_ldargl : Jit.jit_typed_value * Jit.jit_gpr_t * Jit.jit_gpr_t * int -> unit
   val jit_ldargp : Jit.jit_typed_value * Jit.jit_gpr_t * Jit.jit_gpr_t *
                    Jit.jit_gpr_t * Jit.jit_gpr_t * Jit.jit_gpr_t * int -> unit
   val jit_ldargp0 : Jit.jit_typed_value * Jit.jit_gpr_t * 
                     Jit.jit_gpr_t * Jit.jit_gpr_t * Jit.jit_gpr_t -> unit
   val jit_long_val : Jit.jit_typed_value * Jit.jit_gpr_t -> unit
   val jit_raise_exn : Jit.jit_typed_value * excn -> unit
   val jit_binop_dnz : (Jit.jit_typed_value * Jit.jit_gpr_t * Jit.jit_gpr_t * Jit.jit_gpr_t -> 'a) ->
                        Jit.jit_typed_value * Jit.jit_gpr_t * Jit.jit_gpr_t * Jit.jit_gpr_t -> 'a
   val jit_val_bool : Jit.jit_typed_value * Jit.jit_gpr_t * Jit.jit_gpr_t -> unit
   val jit_val_long : Jit.jit_typed_value * Jit.jit_gpr_t -> unit
   val fromWord8ArraySlice : Word8ArraySlice.slice -> word
   val toWord8ArraySlice : word -> Word8ArraySlice.slice
   val toLargeWord   : word -> largeword
   val toLargeWordX  : word -> largeword
   val fromLargeWord : largeword -> word
   val toLarge   : word -> largeword
   val toLargeX  : word -> largeword
   val fromLarge : largeword -> word
end
