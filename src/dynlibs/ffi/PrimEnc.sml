structure PrimEnc :> PrimEnc =
struct
      local
         open Jit
         val wsz = WORDSIZE div 8
      in
         type excn = Word.word
         val exnDiv = 0w28
         fun jit_raise_exn (jit_, excn) =
             let val raiseprimitive0 = Jit.Pointer (Ffi.svec_getcptrvalue Ffi.raiseprimitive0)
                 val _ = jit_prepare (jit_)
                 val _ = jit_pushargi_u (jit_, excn)
                 val _ = jit_finishi (jit_,raiseprimitive0)
             in ()
             end
         fun jit_binop_dnz jit_binop (jit_, r3, r1, r2) =
            let open Jit
                val label = jit_bnei (jit_, r2, 0)
                val _ = jit_raise_exn(jit_, exnDiv)
                val () = jit_patch (jit_, label)
            in jit_binop (jit_, r3, r1, r2)
            end
         fun jit_atom (jit_, v1, v2) =
            let val first_atoms = Pointer (Ffi.svec_getcptrvalue Ffi.first_atoms_)
                val _ = jit_muli (jit_, v2, v2, wsz)
                val _ = jit_movi_p (jit_, v1, first_atoms)
                val _ = jit_addi (jit_, v1, v1, wsz * 1)
                val _ = jit_addr (jit_, v1, v1, v2)
            in ()
            end
         val jit_val_bool = jit_atom
         fun jit_val_long (jit_, v1) =
            let val _ = jit_lshi (jit_, v1, v1, 1)
                val _ = jit_addi (jit_, v1, v1, 1)        (* v1 = Val_long(v1) *)
            in ()
            end
         fun jit_long_val (jit_, v0) =
            let val _  = jit_rshi (jit_, v0, v0, 1)       (* v0 = Long_val(v0) *)
            in ()
            end
         fun jit_ldargl (jit_, v0, r1, n) =
            let val _  = jit_ldxi (jit_, v0, r1, wsz * n) (* v0 = Field(r1,n) *)
                val _  = jit_long_val (jit_, v0)          (* v0 = Long_val(v0) *)
            in ()
            end
      end
end
