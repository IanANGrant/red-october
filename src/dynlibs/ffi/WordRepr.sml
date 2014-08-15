(* Word -- SML Basis Library 1994-11-01, 1995-04-06, 1995-07-12, 
   1996-04-01, 1999-08-05, 2000-10-24 *)

(* This unit relies on two's complement representation *)

type word = Word8ArraySlice.slice;

val wordSize = Jit.WORDSIZE;

local

val WORDSIZE = Jit.WORDSIZE
val wsz = WORDSIZE div 8

fun dnz jit_binop (jit_, r3, r1, r2) =
   let open Jit
       val raiseprimitive0 = Jit.Pointer (Ffi.svec_getcptrvalue Ffi.raiseprimitive0)
       val ref' = jit_bnei (jit_, r2, 0);
       val _ = jit_prepare (jit_);
       val _ = jit_pushargi_u (jit_, Word.fromInt 28);
       val _ = jit_finishi (jit_,raiseprimitive0);

       val () = jit_patch (jit_, ref');
       val _ = jit_binop (jit_, r3, r1, r2);
   in ()
   end

fun binop jit_binop = 
let open Jit
   val jit_ = jit_new_state ();
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

   val _ = jit_binop (jit_, R2, R1, R2);

   val _  = jit_ldxi (jit_, V1, V0, 0); (* V1 = Field(v,0) *)
   val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
   val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
   val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
   val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
   val _  = jit_stxr (jit_, R0, V2, R2); (* *(Field(V1,0)+Field(V1,1)) = R2 *)

   val _ = jit_ret(jit_);

   val slccallptr = jit_emit (jit_);

   val slccall : word * word * word -> unit = Ffi.app1 slccallptr
in
   slccall
end

fun unop jit_unop = 
let open Jit
   val jit_ = jit_new_state ();
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

   val slccall : word * word -> unit = Ffi.app1 slccallptr
in
   slccall
end

val toInt = 
let open Jit
   val jit_ = jit_new_state ();
   val slccall = jit_label (jit_);
   val () = jit_prolog (jit_);
   val v = jit_arg (jit_);

   val () = jit_getarg (jit_, V0, v);
   val _  = jit_ldxi (jit_, R1, V0, 0); (* R1 = Field(V0,0) *)
   val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
   val _  = jit_ldxi (jit_, R0, V0, wsz * 1); (* R0 = Field(V0,1) *)
   val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
   val _  = jit_ldxr (jit_, R1, V2, R0); (* R1 = *(Field(R1,0)+Field(V0,1)) *)

   val _ = jit_lshi (jit_, R1, R1, 1);
   val _ = jit_addi (jit_, R1, R1, 1); (* Val_long(R1) *)

   val _ = jit_retr(jit_,R1);

   val tointptr = jit_emit (jit_);

   val tointcall : word -> int = Ffi.app1 tointptr
in
   tointcall
end

val fromInt_ =
let open Jit
   val jit_ = jit_new_state ();
   val slccall = jit_label (jit_);
   val () = jit_prolog (jit_);
   val v = jit_arg (jit_);

   val () = jit_getarg (jit_, V0, v);

   val _  = jit_ldxi (jit_, R2, V0, 1 * wsz); (* R2 = Field(v,1) *)
   val _  = jit_rshi (jit_, R2, R2, 1); (* Long_val(R2) *)

   val _  = jit_ldxi (jit_, V1, V0, 0); (* V1 = Field(v,0) *)
   val _  = jit_ldxi (jit_, R1, V1, 0); (* R1 = Field(V1,0) *)
   val _  = jit_ldxi (jit_, V2, R1, 0); (* V2 = Field(R1,0) *)
   val _  = jit_ldxi (jit_, R0, V1, wsz * 1); (* R0 = Field(V1,1) *)
   val _  = jit_rshi (jit_, R0, R0, 1); (* Long_val(R0) *)
   val _  = jit_stxr (jit_, R0, V2, R2); (* *(Field(V1,0)+Field(V1,1)) = R2 *)

   val _ = jit_ret(jit_);

   val slccallptr = jit_emit (jit_);

   val slccall : word * int -> unit = Ffi.app1 slccallptr
in
   slccall
end

fun jit_atom (jit_, r1, r2) =
   let open Jit
       val first_atoms = Pointer (Ffi.svec_getcptrvalue Ffi.first_atoms_)
       val _ = jit_muli (jit_, r2, r2, wsz)
       val _ = jit_movi_p (jit_, r1, first_atoms)
       val _ = jit_addi (jit_, r1, r1, wsz * 1)
       val _ = jit_addr (jit_, r1, r1, r2)
   in ()
   end

fun relop jit_relop = 
let open Jit
   val jit_ = jit_new_state ();
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

   val slccall : word * word -> bool = Ffi.app1 slccallptr
in
   slccall
end

val slcadd = binop Jit.jit_addr
val slcsub = binop Jit.jit_subr
val slcmul = binop Jit.jit_mulr
val slcdiv = binop (dnz Jit.jit_divr_u)
val slcmod = binop (dnz Jit.jit_remr_u)
val slcrsh = binop Jit.jit_rshr_u
val slcrsh_u = binop Jit.jit_rshr
val slclsh = binop Jit.jit_lshr

val slcand = binop Jit.jit_andr
val slcor = binop Jit.jit_orr
val slcxor = binop Jit.jit_xorr
val slcnot = unop Jit.jit_comr
val slcneg = unop Jit.jit_negr
val slceq = relop Jit.jit_eqr
val slclt = relop Jit.jit_ltr_u
val slcle = relop Jit.jit_ler_u
val slcgt = relop Jit.jit_gtr_u
val slcge = relop Jit.jit_ger_u

fun mkVal () = Word8ArraySlice.full (Word8Array.array(wsz,0w0));

fun applybin opfn (x,y) =
   let val res = mkVal ()
   in opfn (res,x,y);
     res
   end;

fun applyun opfn x =
   let val res = mkVal ()
   in opfn (res,x);
     res
   end;

in
    val toIntX = toInt;
    val fromInt = applyun fromInt_
    val toLargeInt = toInt;
    val toLargeIntX = toInt;
    val fromLargeInt = fromInt;

    prim_val toLargeWord   : word -> word = 1 "identity";
    prim_val toLargeWordX  : word -> word = 1 "identity";
    prim_val fromLargeWord : word -> word = 1 "identity";

    prim_val toLarge   : word -> word = 1 "identity";
    prim_val toLargeX  : word -> word = 1 "identity";
    prim_val fromLarge : word -> word = 1 "identity";

    fun orb (x, y)  = applybin slcor (x, y)
    fun andb (x, y) = applybin slcand (x, y)
    fun xorb (x, y) = applybin slcxor (x, y)
    fun notb x      = applyun slcnot x

    val ~ = fn w => applyun slcneg w

    fun << (w, k) = 
	if toInt k >= WORDSIZE orelse toInt k < 0 then fromInt 0
	else applybin slclsh (w,k)

    fun >> (w, k) = 
	if toInt k >= WORDSIZE orelse toInt k < 0 then fromInt 0
	else applybin slcrsh_u (w,k)

    fun ~>> (w, k) = 
	if toInt k >= WORDSIZE orelse toInt k < 0 then 
	    if toInt w >= 0 then	(* msbit = 0 *)
		fromInt 0
	    else			(* msbit = 1 *)
		fromInt ~1
	else	
	     applybin slcrsh (w,k)

    val op *    : word * word -> word = applybin slcmul;
    val op +    : word * word -> word = applybin slcadd;
    val op -    : word * word -> word = applybin slcsub;
    val op div  : word * word -> word = applybin slcdiv;
    val op mod  : word * word -> word = applybin slcmod;

    val MAXPOS = (<< (fromInt 1,fromInt (op Int.- (WORDSIZE,2)))) - fromInt 1;

    local 
      open StringCvt
      fun skipWSget getc source = getc (dropl Char.isSpace getc source)

      (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
      fun decval c = fromInt (Char.ord c) - fromInt 48;
      fun hexval c = 
	  if #"0" <= c andalso c <= #"9" then 
	      fromInt (Char.ord c) - fromInt 48
	  else 
	      (fromInt (Char.ord c) - fromInt 55) mod (fromInt 32);

      fun prhex i = 
	  if toInt i < 10 then Char.chr(toInt (i + fromInt 48))
	  else Char.chr(toInt (i + fromInt 55));

      fun conv radix i = 
	  let fun h n res = 
		  if slceq (n, fromInt 0) then res
		  else h (n div radix) (prhex (n mod radix) :: res)
	      fun tostr n = h (n div radix) [prhex (n mod radix)]
	  in String.implode (tostr i) end

    in
      fun scan radix getc source =
	  let open StringCvt
	      val source = skipWS getc source
	      val (isDigit, factor) = 
		  case radix of
		      BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
		    | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
		    | DEC => (Char.isDigit,                          10)
		    | HEX => (Char.isHexDigit,                       16)
	      fun dig1 NONE              = NONE
		| dig1 (SOME (c1, src1)) = 
		  let fun digr res src = 
		          case getc src of
			      NONE           => SOME (res, src)
			    | SOME (c, rest) => 
				  if isDigit c then 
				      digr (fromInt factor * res + hexval c) 
				      rest
				  else SOME (res, src)
		  in 
		      if isDigit c1 then digr (hexval c1) src1 
		      else NONE 
		  end
	      fun getdigs after0 src = 
		  case dig1 (getc src) of
		      NONE => SOME(fromInt 0, after0)
		    | res  => res
	      fun hexprefix after0 src =
		  if radix <> HEX then getdigs after0 src
		  else
		      case getc src of
			  SOME(#"x", rest) => getdigs after0 rest
			| SOME(#"X", rest) => getdigs after0 rest
			| SOME _           => getdigs after0 src
			| NONE => SOME(fromInt 0, after0)
	  in 
	      case getc source of
		  SOME(#"0", after0) => 
		      (case getc after0 of 
			   SOME(#"w", src2) => hexprefix after0 src2 
			 | SOME _           => hexprefix after0 after0 
			 | NONE             => SOME(fromInt 0, after0))
		| SOME _ => dig1 (getc source)
		| NONE   => NONE 
	  end;

      fun fmt BIN = conv (fromInt  2)
	| fmt OCT = conv (fromInt  8)
	| fmt DEC = conv (fromInt 10)
	| fmt HEX = conv (fromInt 16)

      fun toString w   = conv (fromInt 16) w
      fun fromString s = scanString (scan HEX) s
    end (* local for string functions *)

    val op >    : word * word -> bool = slcgt;
    val op >=   : word * word -> bool = slcge;
    val op <    : word * word -> bool = slclt;
    val op <=   : word * word -> bool = slcle;

    fun min(w1 : word, w2) = if w1 > w2 then w2 else w1;
    fun max(w1 : word, w2) = if w1 > w2 then w1 else w2;
    fun compare (x, y: word) = 
	if x<y then LESS else if x>y then GREATER else EQUAL;

    fun toInt w = 
	if w > MAXPOS then raise Overflow
	else toIntX w

end
