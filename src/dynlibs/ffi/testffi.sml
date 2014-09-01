load "Jit";

open Jit

val _ = jit_set_memory_functions Ffi.my_alloc Ffi.my_realloc Ffi.my_free;

val () = init_jit argv0;

val () = Ffi.ffi_report_alloc "after init_jit";

val jit_ = jit_new_state ();

fun jit_fprolog jit_ =
   jit_note (jit_,NULLp,0w0)
   before jit_prolog (jit_);

val wsz = WORDSIZE div 8
val fib = jit_fprolog (jit_);
val n = jit_arg (jit_);

val () = jit_getarg (jit_, V0, n);

val ref' = jit_blti (jit_, V0, 2);
val _ = jit_subi (jit_, V1, V0, 1);
val _ = jit_subi (jit_, V2, V0, 2);

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
val _ = jit_epilog (jit_);

val fibcall = jit_fprolog (jit_);
val v = jit_arg (jit_);

val () = jit_getarg (jit_, V0, v);
val _  = jit_rshi (jit_, V1, V0, 1); (* Long_val(v) *)

val _ = jit_prepare (jit_);
val _ = jit_pushargr (jit_, V1);
val call = jit_finishi (jit_,NULLp);
val _ = jit_patch_at (jit_, call, fib);
val _ = jit_retval (jit_, R0);

val _ = jit_lshi (jit_, R0, R0, 1);
val _ = jit_addi (jit_, R0, R0, 1); (* Val_long(R0) *)
val _ = jit_retr (jit_, R0);
val _ = jit_epilog (jit_);

val veccall = jit_fprolog (jit_);
val v = jit_arg (jit_);

val () = jit_getarg (jit_, V0, v);
val _ = jit_ldxi (jit_, V1, V0, wsz * 1);
val _ = jit_rshi (jit_, V1, V1, 1); (* Long_val(v[1]) *)
val _ = jit_ldxi (jit_, V2, V0, wsz * 2);
val _ = jit_rshi (jit_, V2, V2, 1); (* Long_val(v[2]) *)
val _ = jit_addr (jit_, R0, V1, V2);
val _ = jit_lshi (jit_, R0, R0, 1);
val _ = jit_addi (jit_, R0, R0, 1); (* Val_long(R0) *)
val _ = jit_stxi (jit_, 0, V0, R0);
val _ = jit_retr (jit_, V0);
val _ = jit_epilog (jit_);

val _ = jit_emit (jit_);
val fibcalladd = jit_address (jit_,fibcall);
val veccalladd = jit_address (jit_,veccall);
val () = jit_clear_state (jit_); 

val fib : int -> int = Ffi.app1 fibcalladd;

val veccall : int * int * int -> int * int * int = Ffi.app1 veccalladd;

val r' = List.map fib [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];

val r'' = veccall (0,11,31);

val () = jit_disassemble (jit_); 

fun exercise_gc 0 = [] | exercise_gc n = n :: exercise_gc (n-1);
val t = exercise_gc 100000;

val () = jit_disassemble (jit_); 

val () = Ffi.ffi_report_alloc "before finish_jit";

val () = finish_jit ();

val () = Ffi.ffi_report_alloc "after finish_jit";

val t' = exercise_gc 100000;

val crashr' = List.map fib [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];

val crashr'' = veccall (0,11,31);

val () = jit_destroy_state (jit_); 

val () = Ffi.ffi_report_alloc "end of testffi";
