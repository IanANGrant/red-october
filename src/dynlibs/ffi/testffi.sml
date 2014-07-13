load "Jit";

val () = Jit.init_jit Jit.argv0;
val jit1_ = Jit.jit_new_state ();

local open Jit
   val jit_ = jit1_;
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

   val _ = print "fib:\n"
   val () = Jit.jit_clear_state (jit_);
   val () = Jit.jit_disassemble (jit_);
in
   val fibsp = Jit.Pointer (Ffi.svec_getcptrvalue fptr)
end;

val jit2_ = Jit.jit_new_state ();

local open Jit
   val jit_ = jit2_;
   val fibcall = jit_label (jit_);
   val () = jit_prolog (jit_);
   val v = jit_arg (jit_);

   val () = jit_getarg (jit_, V0, v);
   val _  = jit_rshi (jit_, V1, V0, 1); (* Long_val(v) *)

   val _ = jit_prepare (jit_);
   val _ = jit_pushargr (jit_, V1);
   val _ = jit_finishi (jit_,fibsp);
   val _ = jit_retval (jit_, R0);
   val _ = jit_lshi (jit_, R0, R0, 1);
   val _ = jit_addi (jit_, R0, R0, 1); (* Val_long(R0) *)
   val _ = jit_retr (jit_, R0);
   val fcallptr = jit_emit (jit_);

   val _ = print "fibcall:\n"
   val () = Jit.jit_clear_state (jit_);
   val () = Jit.jit_disassemble (jit_);
in
   val fib : int -> int = Ffi.app1 fcallptr
end

val r' = List.map fib [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];

val jit3_ = Jit.jit_new_state ();

local open Jit
   val wsz = Jit.WORDSIZE div 8
   val jit_ = jit3_;
   val veccall = jit_label (jit_);
   val () = jit_prolog (jit_);
   val v = jit_arg (jit_);

   val () = jit_getarg (jit_, V0, v);
   val _  = jit_ldxi (jit_, V1, V0, wsz * 1);
   val _  = jit_rshi (jit_, V1, V1, 1); (* Long_val(v[1]) *)
   val _  = jit_ldxi (jit_, V2, V0, wsz * 2);
   val _  = jit_rshi (jit_, V2, V2, 1); (* Long_val(v[2]) *)
   val _ = jit_addr (jit_, R0, V1, V2);
   val _ = jit_lshi (jit_, R0, R0, 1);
   val _ = jit_addi (jit_, R0, R0, 1); (* Val_long(R0) *)
   val _  = jit_stxi (jit_, 0, V0, R0);
   val _ = jit_retr (jit_, V0);
   val veccallptr = jit_emit (jit_);

   val _ = print "veccall:\n"
   val () = Jit.jit_clear_state (jit_);
   val () = Jit.jit_disassemble (jit_);
in
   val veccall : int * int * int -> int * int * int = Ffi.app1 veccallptr
end

val r'' = veccall (0,11,31);

fun binop jit_binop = 
let open Jit
   val wsz = Jit.WORDSIZE div 8
   val jit_ = Jit.jit_new_state ();
   val slccall = jit_label (jit_);
   val () = jit_prolog (jit_);
   val v = jit_arg (jit_);

   val () = jit_getarg (jit_, V0, v);
   val _  = jit_ldxi (jit_, V1, V0, wsz * 1); (* V1 = Field(v,1) *)
   val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
   val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
   val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
   val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
   val _  = jit_ldxr (jit_, R1, V2, R0); (* R1 = *(Field(V1,0)+Field(V1,1)) *)

   val _  = jit_ldxi (jit_, V1, V0, wsz * 2); (* V1 = Field(v,2) *)
   val _  = jit_ldxi (jit_, R2, V1, 0); (* R2 = Field(V1,0) *)
   val _  = jit_ldxi (jit_, V2, R2, 0); (* V2 = Field(R2,0) *)
   val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
   val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
   val _  = jit_ldxr (jit_, R2, V2, R0); (* R2 = *(Field(V1,0)+Field(V1,1)) *)

   val _ = jit_binop (jit_, R2, R2, R1);

   val _  = jit_ldxi (jit_, V1, V0, 0); (* V1 = Field(v,0) *)
   val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
   val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
   val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
   val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
   val _  = jit_stxr (jit_, R0, V2, R2); (* *(Field(V1,0)+Field(V1,1)) = R2 *)

   val _ = jit_ret(jit_);

   val slccallptr = jit_emit (jit_);

   val _ = print "slccall:\n"
   val () = Jit.jit_clear_state (jit_);
   val () = Jit.jit_disassemble (jit_);
   val slccall : Word8ArraySlice.slice * Word8ArraySlice.slice * Word8ArraySlice.slice ->
                 unit = Ffi.app1 slccallptr
in
   slccall
end

fun unop jit_unop = 
let open Jit
   val wsz = Jit.WORDSIZE div 8
   val jit_ = Jit.jit_new_state ();
   val slccall = jit_label (jit_);
   val () = jit_prolog (jit_);
   val v = jit_arg (jit_);

   val () = jit_getarg (jit_, V0, v);
   val _  = jit_ldxi (jit_, V1, V0, wsz * 1); (* V1 = Field(v,1) *)
   val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
   val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
   val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
   val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
   val _  = jit_ldxr (jit_, R1, V2, R0); (* R1 = *(Field(V1,0)+Field(V1,1)) *)

   val _ = jit_unop (jit_, R2, R1);

   val _  = jit_ldxi (jit_, V1, V0, 0); (* V1 = Field(v,0) *)
   val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
   val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
   val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
   val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
   val _  = jit_stxr (jit_, R0, V2, R2); (* *(Field(V1,0)+Field(V1,1)) = R2 *)

   val _ = jit_ret(jit_);

   val slccallptr = jit_emit (jit_);

   val _ = print "slccall:\n"
   val () = Jit.jit_clear_state (jit_);
   val () = Jit.jit_disassemble (jit_);
   val slccall : Word8ArraySlice.slice * Word8ArraySlice.slice ->
                 unit = Ffi.app1 slccallptr
in
   slccall
end

fun jit_atom (jit_, r1, r2) =
   let open Jit
       val wsz = WORDSIZE div 8
       val first_atoms = Pointer (Ffi.svec_getcptrvalue Ffi.first_atoms_)
       val _ = jit_muli (jit_, r2, r2, wsz)
       val _ = jit_movi_p (jit_, r1, first_atoms)
       val _ = jit_addi (jit_, r1, r1, wsz * 1)
       val _ = jit_addr (jit_, r1, r1, r2)
   in ()
   end

fun relop jit_relop = 
let open Jit
   val wsz = Jit.WORDSIZE div 8
   val jit_ = Jit.jit_new_state ();
   val slccall = jit_label (jit_);
   val () = jit_prolog (jit_);
   val v = jit_arg (jit_);

   val () = jit_getarg (jit_, V0, v);
   val _  = jit_ldxi (jit_, V1, V0, 0); (* V1 = Field(v,0) *)
   val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
   val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
   val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
   val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
   val _  = jit_ldxr (jit_, R1, V2, R0); (* R1 = *(Field(V1,0)+Field(V1,1)) *)

   val _  = jit_ldxi (jit_, V1, V0, wsz * 1); (* V1 = Field(v,1) *)
   val _  = jit_ldxi (jit_, R2, V1, 0); (* R2 = Field(V1,0) *)
   val _  = jit_ldxi (jit_, V2, R2, 0); (* V2 = Field(R2,0) *)
   val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
   val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
   val _  = jit_ldxr (jit_, R2, V2, R0); (* R2 = *(Field(V1,0)+Field(V1,1)) *)

   val _ = jit_relop (jit_, R2, R1, R2);
   val _ = jit_atom (jit_, R1, R2);
   val _ = jit_retr(jit_, R1);

   val slccallptr = jit_emit (jit_);

   val _ = print "slccall:\n"
   val () = Jit.jit_clear_state (jit_);
   val () = Jit.jit_disassemble (jit_);
   val slccall : Word8ArraySlice.slice * Word8ArraySlice.slice ->
                 bool = Ffi.app1 slccallptr
in
   slccall
end

val slcadd = binop Jit.jit_addr
val slcsub = binop Jit.jit_subr
val slcdiv = binop Jit.jit_divr
val slcmod = binop Jit.jit_remr
val slceq = relop Jit.jit_eqr
val slclt = relop Jit.jit_ltr
val slcle = relop Jit.jit_ler
val slcgt = relop Jit.jit_gtr
val slcge = relop Jit.jit_ger
val slccom = unop Jit.jit_comr

val s1 = Word8ArraySlice.full (Word8Array.fromList [0wx01,0wx01,0wx0e,0wx00])
val s2 = Word8ArraySlice.full (Word8Array.fromList [0wx02,0wx02,0wx01,0wx00])
val s3 = Word8ArraySlice.full (Word8Array.fromList [0wx00,0wx00,0wx00,0wx00])
val s4 = Word8ArraySlice.full (Word8Array.fromList [0wx03,0wx03,0wx0f,0wx00])

fun toList s =
   Word8ArraySlice.foldr (fn (w,a) => w::a) [] s;

val _ = slcadd (s3,s1,s2);
val r''' = (slceq (s3,s4),slceq (s3,s1),slcle (s3,s1),slcge (s3,s1),slclt (s3,s1),slcgt (s3,s1));

val r'''' = toList s3;

val _ = slccom (s1,s2);
val r5 = toList s1;
 
val () = Jit.jit_destroy_state (jit3_);
val () = Jit.jit_destroy_state (jit2_);
val () = Jit.jit_destroy_state (jit1_);
val () = Jit.finish_jit ();
