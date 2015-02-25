val _ = load "Lightning32";
val _ = load "MappedWord8Array";
val _ = load "MappedWord8ArraySlice";

val _ = load "IntInf";

    val slfh =
       Dynlib.dlopen {lib = "",
                      flag = Dynlib.RTLD_LAZY,
                      global = false };

    val dlh = Dynlib.dlopen { lib = "libmgmp.so",
		       flag = Dynlib.RTLD_LAZY, 
		       global = false }

fun slfsym s = Dynlib.dlsym slfh s;
fun slfsymp s = Dynlib.cptr (slfsym s);

fun dlhsym s = Dynlib.dlsym dlh s;
fun dlhsymp s = Dynlib.cptr (dlhsym s);

open Lightning32;

val sha256_initp   = dlhsymp "sha256_init";
val sha256_updatep = dlhsymp "sha256_update";
val sha256_finalp  = dlhsymp "sha256_final";

val () = init_jit();

val ws = WORDSIZE;
val bo = BYTE_ORDER;
val le = LITTLE_ENDIAN;
val be = BIG_ENDIAN;

val jit_ = jit_new_state();

fun jit_fprolog jit_ =
   jit_note (jit_,NULL,0w0)
   before jit_prolog (jit_);

   val wsz = Word.fromInt (WORDSIZE div 8);

   val update = jit_fprolog (jit_);
   val v = jit_arg (jit_);

   val () = jit_getarg (jit_, V0, v)
   val _ = jit_ldxi (jit_, V2, V0, wsz * 0w0) (* V2 = Field(v,0)   *)
   val _ = jit_ldxi (jit_, V1, V0, wsz * 0w1) (* V1 = Field(v,1)   *)
   val _ = jit_ldxi (jit_, V0, V0, wsz * 0w2) (* V0 = Field(v,2)   *)
   val _ = jit_rshi (jit_, V0, V0, 0w1)       (* V0 = Long_val(V0) *)
   val _ = jit_prepare (jit_)
   val _ = jit_pushargr (jit_, V2)
   val _ = jit_pushargr (jit_, V1)
   val _ = jit_pushargr (jit_, V0)
   val _ = jit_finishi (jit_, sha256_updatep) 
   val _ = jit_retval (jit_, R0);

   val _ = jit_lshi (jit_, R0, R0, 0w1);
   val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
   val _ = jit_retr (jit_, R0)
   val _ = jit_epilog (jit_);

   val init = jit_fprolog (jit_);

   val v = jit_arg (jit_);

   val () = jit_getarg (jit_, V2, v)
   val _ = jit_prepare (jit_)
   val _ = jit_pushargr (jit_, V2)
   val _ = jit_finishi (jit_, sha256_initp) 
   val _ = jit_retval (jit_, R0);

   val _ = jit_lshi (jit_, R0, R0, 0w1);
   val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
   val _ = jit_retr (jit_, R0)
   val _ = jit_epilog (jit_);

   val final = jit_fprolog (jit_);

   val v = jit_arg (jit_);

   val () = jit_getarg (jit_, V0, v)
   val _ = jit_ldxi (jit_, V2, V0, wsz * 0w0) (* V2 = Field(v,0)   *)
   val _ = jit_ldxi (jit_, V1, V0, wsz * 0w1) (* V1 = Field(v,1)   *)

   val _ = jit_prepare (jit_)
   val _ = jit_pushargr (jit_, V2)
   val _ = jit_pushargr (jit_, V1)
   val _ = jit_finishi (jit_, sha256_finalp) 
   val _ = jit_retval (jit_, R0);

   val _ = jit_lshi (jit_, R0, R0, 0w1);
   val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
   val _ = jit_retr (jit_, R0)
   val _ = jit_epilog (jit_);

val _ = jit_emit (jit_);

val initadd   = jit_address (jit_, init);
val updateadd = jit_address (jit_, update);
val finaladd  = jit_address (jit_, final);

val () = jit_clear_state (jit_);

val sha256_init : Dynlib.cptr -> int = app1 initadd;
val sha256_update : Dynlib.cptr * Dynlib.cptr * int -> int = app1 updateadd;
val sha256_final : Dynlib.cptr * Dynlib.cptr -> int = app1 finaladd;

val () = jit_disassemble (jit_); 

(*
struct sha256_state {
	u64 count;
	u32 state[SHA256_DIGEST_SIZE / 4];
	u8 buf[SHA256_BLOCK_SIZE];
}; *)

val ctxtsize = 8+32+64
val ctxt = MappedWord8Array.array (ctxtsize,0w0);
val ctxtp = MappedWord8Array.get_cptr ctxt;

val buffer = MappedWord8Array.array (1024,0w0);
val sha256sum = MappedWord8Array.array (32,0w0);
val vecFromString : string -> Word8Vector.vector = Obj.magic;
val loadString = fn arr => fn s => MappedWord8Array.copyVec {di=0,dst=arr,src=vecFromString s};

val _ = loadString buffer "abc";
val bufp = MappedWord8Array.get_cptr buffer;
val sump = MappedWord8Array.get_cptr sha256sum;
val _ = sha256_init (ctxtp);
val rv = sha256_update (ctxtp,bufp,3);
val rv' = sha256_final (ctxtp,sump);

(* echo -n "abc" | sha256sum *)

val strsum = MappedWord8Array.foldl (fn (w,a) => a^(Word8.toString w)) "" sha256sum;
