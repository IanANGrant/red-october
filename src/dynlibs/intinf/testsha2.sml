val _ = load "Lightning32";
val _ = load "StaticWord8ArraySlice";
val _ = load "StaticBuffer";

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

val toHexStr = IntInf.fmt StringCvt.HEX;
val num = IntInf.fromInt (Char.ord #"*");
val fromHexString = StringCvt.scanString (IntInf.scan StringCvt.HEX)

val SOME hash = fromHexString "1b7c99b95cc6fad6d88b2c3fdd0d9faab2a4fd1de0d6a880bd2261ac2592a222";
val hash'n = toHexStr hash;

val (slc,nwords,sgn) = IntInf.export IntInf.rawformat hash;

val w8vsl = Word8Vector.app (fn (w) => (print ((Word8.toString w)^" "))) (Word8ArraySlice.vector slc);

val hash' = IntInf.import IntInf.rawformat (slc,nwords,sgn);
val hash'n' = toHexStr hash';

fun mkLargeint () =
   let val mpz = IntInf.init2 (0x100,0x0);
       val mpzp = IntInf.getCptr mpz;
   in mpzp
   end

val wvarr = Array.tabulate 
              (nwords,
                 fn i => (Word8ArraySlice.vector (Word8ArraySlice.subslice (slc,i*4,SOME 4))));

val num2 = IntInf.orb(IntInf.<<(num, 32),IntInf.+(num, IntInf.fromInt 1));
val num3 = IntInf.orb(IntInf.<<(num2,64),IntInf.+(num2,IntInf.fromInt 1));
val num3'n = toHexStr num3;

val (slc,nwords,sgn) = IntInf.export IntInf.rawformat num3;

val wvl = Array.foldl op :: [] wvarr;
val num' = IntInf.import IntInf.rawformat (slc,nwords,sgn);
val num'n = toHexStr num';

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
   val _ = jit_rshi (jit_, V1, V1, 0w1)       (* V1 = Long_val(V1) *)
   val _ = jit_prepare (jit_)
   val _ = jit_pushargr (jit_, V2)
   val _ = jit_pushargr (jit_, V1)
   val _ = jit_finishi (jit_, sha256_updatep) 
   val _ = jit_retval (jit_, R0);

   val _ = jit_lshi (jit_, R0, R0, 0w1);
   val _ = jit_addi (jit_, R0, R0, 0w1); (* Val_long(R0) *)
   val _ = jit_retr (jit_, R0)
   val _ = jit_epilog (jit_);

   val init = jit_fprolog (jit_);

   val _ = jit_prepare (jit_)
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
   val _ = jit_prepare (jit_)
   val _ = jit_pushargr (jit_, V2)
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

val sha256_init : unit -> int = app1 initadd;
val sha256_update : cptr * int -> int = app1 updateadd;
val sha256_final : cptr -> int = app1 finaladd;

val () = jit_disassemble (jit_); 
