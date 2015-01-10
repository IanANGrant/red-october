val _ = load "Lightning32";

val slfh =
       Dynlib.dlopen {lib = "",
                      flag = Dynlib.RTLD_LAZY,
                      global = false };

fun slfsym s = Dynlib.dlsym slfh s;
fun slfsymp s = Dynlib.cptr (slfsym s);

open Lightning32;

val () = init_jit();

val ws = WORDSIZE;
val bo = BYTE_ORDER;
val le = LITTLE_ENDIAN;
val be = BIG_ENDIAN;

val jit_ = jit_new_state();

val () = jit_prolog jit_;
val v =  jit_arg jit_;
val () = jit_getarg_i (jit_, R0, v);
val _ =  jit_rshi_u (jit_, R1, R0, 0w1);
val _ =  jit_addi (jit_, R0, R1, 0w2);
val _ =  jit_lshi (jit_, R1, R0, 0w1);
val _ =  jit_addi (jit_, R0, R1, 0w1);
val () = jit_retr (jit_, R0);
val cp = jit_emit jit_;

val p2 : int -> int
   = app1 cp;

val () = jit_disassemble jit_;

val jit2_ = jit_new_state();

val () = jit_prolog jit2_;
val v  = jit_arg jit2_;
val () = jit_getarg_i (jit2_, R0, v);
val _  = jit_ldr_d (jit2_, F0, R0);
val _  = jit_addi_d (jit2_, F1, F0, 42.0);
val _  = jit_prepare (jit2_);
val _  = jit_pushargr_d (jit2_, F1);
val _  = jit_finishi (jit2_, slfsymp "copy_double");
val _  = jit_retval_i (jit2_, R0);
val () = jit_retr (jit2_, R0);
val cp = jit_emit jit2_;

val p42 : real -> real
   = app1 cp;

val () = jit_disassemble jit2_;

val jit_ = jit_new_state();

fun jit_fprolog jit_ =
   jit_note (jit_,NULL,0w0)
   before jit_prolog (jit_);

val wsz = Word.fromInt (WORDSIZE div 8);
val fib = jit_fprolog (jit_);
val n = jit_arg (jit_);

val () = jit_getarg (jit_, V0, n);

val ref' = jit_blti (jit_, V0, 0w2);
val _ = jit_subi (jit_, V1, V0, 0w1);
val _ = jit_subi (jit_, V2, V0, 0w2);

val _ = jit_prepare (jit_);
val _ = jit_pushargr (jit_, V1);
val call = jit_finishi (jit_,NULL);
val _ = jit_patch_at (jit_, call, fib);
val _ = jit_retval(jit_, V1);

val _ = jit_prepare (jit_);
val _ = jit_pushargr (jit_, V2);
val call = jit_finishi (jit_,NULL);
val _ = jit_patch_at (jit_, call, fib);
val _ = jit_retval(jit_, V2);

val _ = jit_addr (jit_, R0, V1, V2);
val _ = jit_retr (jit_, R0);

val () = jit_patch (jit_, ref');
val _ = jit_movi (jit_, R0, 0w1);
val _ = jit_retr (jit_, R0);
val _ = jit_epilog (jit_);

val fibcall = jit_fprolog (jit_);
val v = jit_arg (jit_);

val () = jit_getarg (jit_, V0, v);
val _  = jit_rshi (jit_, V1, V0, 0w1); (* Long_val(v) *)

val _ = jit_prepare (jit_);
val _ = jit_pushargr (jit_, V1);
val call = jit_finishi (jit_,NULL);
val _ = jit_patch_at (jit_, call, fib);
val _ = jit_retval (jit_, R0);

val _ = jit_lshi (jit_, R0, R0, 0w1);
val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
val _ = jit_retr (jit_, R0);
val _ = jit_epilog (jit_);

val veccall = jit_fprolog (jit_);
val v = jit_arg (jit_);

val () = jit_getarg_i (jit_, V0, v);
val _ = jit_ldxi (jit_, V1, V0, wsz * 0w1);
val _ = jit_rshi (jit_, V1, V1, 0w1); (* Long_val(v[1]) *)
val _ = jit_ldxi (jit_, V2, V0, wsz * 0w2);
val _ = jit_rshi (jit_, V2, V2, 0w1); (* Long_val(v[2]) *)
val _ = jit_addr (jit_, R0, V1, V2);
val _ = jit_lshi (jit_, R0, R0, 0w1);
val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
val _ = jit_stxi (jit_, 0w0, V0, R0);
val _ = jit_retr (jit_, V0);
val _ = jit_epilog (jit_);

val _ = jit_emit (jit_);
val fibcalladd = jit_address (jit_,fibcall);
val veccalladd = jit_address (jit_,veccall);
val () = jit_clear_state (jit_);

val fib : int -> int = app1 fibcalladd;

val veccall : int * int * int -> int * int * int = app1 veccalladd;

val r' = List.map fib [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];

val r'' = veccall (0,11,31);

val () = jit_disassemble (jit_); 

val crtip = slfsymp "sys_catch_rtintr";
val cip = slfsymp "sys_catch_break";

val catch_interrupt : bool -> unit = app1 cip;
val catch_rtintr : bool -> unit = app1 crtip;

val () = catch_rtintr true;
