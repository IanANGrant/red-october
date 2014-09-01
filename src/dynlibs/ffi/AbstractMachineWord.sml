functor AbstractMachineWord(structure Enc : WordEnc
                            structure Ops : WordOps
                                where type state = Jit.jit_typed_value
                                  and type gpr = Jit.jit_gpr_t)
   :> WordPrim where type word = Enc.word
                and  type largeword = Enc.largeword =
struct
   type word = Enc.word
   type largeword = Enc.largeword
   val wordSize = Enc.wordSize
   local
      fun binop jit_binop =
         let open Jit
            val jit_ = jit_new_state ()
            val prmcall = jit_note (jit_, NULLp, 0w0);
            val () = jit_prolog (jit_)
            val v = jit_arg (jit_)
            val () = jit_getarg (jit_, R0, v)
            val _ = Enc.jit_ldargp (jit_, R2, V0, R0, R1, V1, 1)
            val _ = jit_ldxr (jit_, V1, R2, V0)
            val _ = Enc.jit_ldargp (jit_, R2, V0, R0, R1, V2, 2)
            val _ = jit_ldxr (jit_, V2, R2, V0)
            val _ = jit_binop (jit_, V2, V1, V2)
            val () = jit_getarg (jit_, R0, v)
            val _ = Enc.jit_ldargp (jit_, R2, V0, R0, R1, V1, 0)
            val _ = jit_stxr (jit_, V0, R2, V2)
            val _ = jit_ret(jit_)
            val prmcallptr = jit_emit (jit_)
            val prmcall : word * word * word -> unit = Ffi.app1 prmcallptr
         in
            prmcall
         end
      fun relop jit_relop = 
         let open Jit
            val jit_ = jit_new_state ()
            val prmcall = jit_note (jit_, NULLp, 0w0);
            val () = jit_prolog (jit_)
            val v = jit_arg (jit_)
            val () = jit_getarg (jit_, R0, v)
            val _ = Enc.jit_ldargp (jit_, R2, V0, R0, R1, V1, 0)
            val _ = jit_ldxr (jit_, V1, R2, V0)
            val _ = Enc.jit_ldargp (jit_, R2, V0, R0, R1, V2, 1)
            val _ = jit_ldxr (jit_, V2, R2, V0)
            val _ = jit_relop (jit_, V2, V1, V2)
            val _ = Enc.jit_val_bool (jit_, V1, V2)
            val _ = jit_retr(jit_, V1)
            val prmcallptr = jit_emit (jit_)
            val prmcall : word * word -> bool = Ffi.app1 prmcallptr
         in
            prmcall
         end
      fun unop jit_unop = 
         let open Jit
            val jit_ = jit_new_state ()
            val prmcall = jit_note (jit_, NULLp, 0w0);
            val () = jit_prolog (jit_)
            val v = jit_arg (jit_)
            val () = jit_getarg (jit_, R0, v)
            val _ = Enc.jit_ldargp (jit_, R2, V0, R0, R1, V1, 1)
            val _ = jit_ldxr (jit_, V1, R2, V0)
            val _ = jit_unop (jit_, V2, V1)
            val () = jit_getarg (jit_, R0, v)
            val _ = Enc.jit_ldargp (jit_, R2, V0, R0, R1, V1, 0)
            val _ = jit_stxr (jit_, V0, R2, V2)
            val _ = jit_ret(jit_)
            val prmcallptr = jit_emit (jit_)
            val prmcall : word * word -> unit = Ffi.app1 prmcallptr
         in
            prmcall
         end
      val toInt = 
         let open Jit
            val jit_ = jit_new_state ()
            val prmcall = jit_note (jit_, NULLp, 0w0);
            val () = jit_prolog (jit_)
            val v = jit_arg (jit_)
            val () = jit_getarg (jit_, R1, v)
            val _ = Enc.jit_ldargp0 (jit_, R2, V0, R1, V1)
            val _ = jit_ldxr (jit_, V1, R2, V0)
            val _ = Enc.jit_val_long(jit_, V1)
            val _ = jit_retr(jit_,V1)
            val tointptr = jit_emit (jit_)
            val tointcall : word -> int = Ffi.app1 tointptr
         in
            tointcall
         end
      val fromInt_ =
         let open Jit
            val jit_ = jit_new_state ()
            val prmcall = jit_note (jit_, NULLp, 0w0);
            val () = jit_prolog (jit_)
            val v = jit_arg (jit_)
            val () = jit_getarg (jit_, R0, v)
            val _  = Enc.jit_ldargl (jit_, V2, R0, 1)
            val () = jit_getarg (jit_, R0, v)
            val _  = Enc.jit_ldargp (jit_, R2, V0, R0, R1, V1, 0)
            val _  = jit_stxr (jit_, V0, R2, V2)
            val _  = jit_ret(jit_)
            val prmcallptr = jit_emit (jit_)
            val prmcall : word * int -> unit = Ffi.app1 prmcallptr
         in
            prmcall
         end
      fun applybin opfn (x,y) =
         let val res = Enc.mkVal ()
         in opfn (res,x,y);
           res
         end
      fun applyun opfn x =
         let val res = Enc.mkVal ()
         in opfn (res,x);
           res
         end
      val bprim    = applybin o binop
      val uprim    = applyun o unop
      val rprim    = relop
   in
      val fromInt = applyun fromInt_
      val toInt = toInt
      val toIntX = toInt
      val toLargeInt = toInt
      val toLargeIntX = toInt
      val fromLargeInt = fromInt
      val fromWord8ArraySlice = Enc.fromWord8ArraySlice
      val toWord8ArraySlice = Enc.toWord8ArraySlice
      val toLargeWord = Enc.toLargeWord
      val toLargeWordX = Enc.toLargeWordX
      val fromLargeWord = Enc.fromLargeWord
      val toLarge = Enc.toLarge
      val toLargeX = Enc.toLargeX
      val fromLarge = Enc.fromLarge
      val op +   = bprim Ops.jit_addr
      val op -   = bprim Ops.jit_subr
      val op *   = bprim Ops.jit_mulr
      val op div = bprim (Enc.jit_binop_dnz Ops.jit_divr_u)
      val op mod = bprim (Enc.jit_binop_dnz Ops.jit_remr_u)
      val op ~>> = bprim Ops.jit_rshr
      val op >>  = bprim Ops.jit_rshr_u
      val op <<  = bprim Ops.jit_lshr
      val andb   = bprim Ops.jit_andr
      val orb    = bprim Ops.jit_orr
      val xorb   = bprim Ops.jit_xorr
      val notb   = uprim Ops.jit_comr
      val op ~   = uprim Ops.jit_negr
      val eq     = rprim Ops.jit_eqr
      val op <   = rprim Ops.jit_ltr_u
      val op <=  = rprim Ops.jit_ler_u
      val op >   = rprim Ops.jit_gtr_u
      val op >=  = rprim Ops.jit_ger_u
      val MAXPOS = (<< (fromInt 1,fromInt (op Int.- (wordSize,2)))) - fromInt 1
   end
end
