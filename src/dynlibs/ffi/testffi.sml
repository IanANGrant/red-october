load "Jit";

val () = Jit.init_jit Jit.argv0;
val jit_ = Jit.jit_new_state ();

local open Jit
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
in
   val fibs = ffi_trampoline "fibs" fptr fibstype mkargs mkretval;
end;

val r = List.map fibs [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];

val () = Jit.jit_clear_state (jit_);
val () = Jit.jit_disassemble (jit_);
val () = Jit.jit_destroy_state (jit_);
val () = Jit.finish_jit ();
