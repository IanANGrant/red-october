(* Word -- SML Basis Library *)
(* Word -- SML Basis Library 1994-11-01, 1995-04-06, 1995-07-12, 
   1996-04-01, 1999-08-05, 2000-10-24 *)

(* This unit relies on two's complement representation *)

functor WordN(structure Prim : Word) :> Word 
    where type word = Prim.word = 
struct
    type word = Prim.word
    type largeword = Prim.largeword

    val wordSize = Prim.wordSize
    val toInt = Prim.toInt;
    val toIntX = Prim.toIntX;
    val fromInt = Prim.fromInt
    val toLargeInt = Prim.toLargeInt;
    val toLargeIntX = Prim.toLargeIntX;
    val fromLargeInt = Prim.fromLargeInt;
    val toLargeWord = Prim.toLargeWord;
    val toLargeWordX = Prim.toLargeWordX;
    val fromLargeWord = Prim.fromLargeWord;
    val toLarge = Prim.toLarge;
    val toLargeX = Prim.toLargeX;
    val fromLarge = Prim.fromLarge;
    val orb = Prim.orb;
    val andb = Prim.andb;
    val xorb = Prim.xorb;
    val notb = Prim.notb;
    val op ~ = Prim.~;
    val op << = Prim.<<;
    val op >> = Prim.>>;
    val op ~>> = Prim.~>>;
    val op *    : word * word -> word = Prim.*;
    val op +    : word * word -> word = Prim.+;
    val op -    : word * word -> word = Prim.-;
    val op div  : word * word -> word = Prim.div;
    val op mod  : word * word -> word = Prim.mod;
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
		  if Prim.compare (n, fromInt 0) = EQUAL then res
		  else h (n div radix) (prhex (n mod radix) :: res)
	      fun tostr n = h (n div radix) [prhex (n mod radix)]
	  in String.implode (tostr i)
          end
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
    val MAXPOS = (<< (fromInt 1,fromInt (op Int.- (wordSize,2)))) - fromInt 1
    val op >    : word * word -> bool = Prim.>;
    val op >=   : word * word -> bool = Prim.>=;
    val op <    : word * word -> bool = Prim.<;
    val op <=   : word * word -> bool = Prim.<=;
    fun min(w1 : word, w2) = if w1 > w2 then w2 else w1;
    fun max(w1 : word, w2) = if w1 > w2 then w1 else w2;
    fun compare (x, y: word) = 
	if x<y then LESS else if x>y then GREATER else EQUAL;
    fun toInt w = 
	if w > MAXPOS then raise Overflow
	else toIntX w
end
