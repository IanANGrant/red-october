type word = word;

val wordSize = 32;

local open Jit
   val () = init_jit argv0;
   val jit_ = jit_new_state ();

   val fib = jit_label (jit_);
   val () = jit_prolog (jit_);
   val n = jit_arg (jit_);

   val () = jit_getarg (jit_, V0, n);

   val ref' = jit_blti (jit_, V0, 2);
   val _  = jit_subi (jit_, V1, V0, 1);
   val _  = jit_subi (jit_, V2, V0, 2);

   val _ = jit_prepare (jit_);
   val _ = jit_pushargr (jit_, V1);
   val call = jit_finishi (jit_,NULLp);
   val _ = jit_patch_at (jit_, call, fib);
   val _ = jit_retval(jit_, V1);

   val _ = jit_prepare (jit_);
   val _ = jit_pushargr (jit_, V2);
   val call = jit_finishi (jit_,NULLp);
   val _ = jit_patch_at (jit_, call, fib);
   val _ = jit_retval(jit_, V2);

   val _ = jit_addr (jit_, R0, V1, V2);
   val _ = jit_retr (jit_, R0);

   val () = jit_patch (jit_, ref');
   val _ = jit_movi (jit_, R0, 1);
   val _ = jit_retr (jit_, R0);

   val fptr = jit_emit (jit_);

   open Ffi
   val fibstype = Function(Integer(Signed,Int), SOME FFI_TYPE_INT);
   fun mkargs n = svec_setvecword (Word.fromInt n)
   fun mkretval v = Word.toInt (svec_getvecword v)

   val fibs = ffi_trampoline "fibs" fptr fibstype mkargs mkretval;
in
end
