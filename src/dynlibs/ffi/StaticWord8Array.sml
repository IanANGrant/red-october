(* Static Word8Array -- SML Basis Library, PS 1994-12-21, 2000-10-25
   Minor changes to primitives to use static arrays, IG 2014-11-11. 
*)

type elem   = Word8.word;
type vector = Word8Vector.vector;

local 
    prim_eqtype array_;

    open Dynlib

    val dlh = Dynlib.dlopen
                {lib = "libmffi.so",
                 flag = Dynlib.RTLD_LAZY,
                 global = false}

    val dlxh = Dynlib.dlopen {lib = "",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }

    val sym = Dynlib.dlsym dlh

    val app1 : string -> 'a -> 'b
         = fn s => Dynlib.app1 (sym s)
    val app2 : string -> 'a -> 'b -> 'c
         = fn s => Dynlib.app2 (sym s)
    val app3 : string -> 'a -> 'b -> 'c -> 'd
         = fn s => Dynlib.app3 (sym s)
    val app4 : string -> 'a -> 'b -> 'c -> 'd -> 'e
         = fn s => Dynlib.app4 (sym s)
    val app5 : string -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
         = fn s => Dynlib.app5 (sym s)

    val array_ : int -> array_
          = app1 "svec_make"

    val svec_wrap_cptr     : Dynlib.cptr -> Dynlib.cptr -> int -> array_
          = app3 "svec_wrap_cptr"

    val svec_wrap_cptr2     : Dynlib.cptr -> Dynlib.cptr -> int -> array_
          = app3 "svec_wrap_cptr2"

    val svec_getbuffercptr : array_ -> Dynlib.cptr
          = app1 "svec_getbuffer"

    val svec_getlength     : array_ -> int         
          = app1 "svec_getlength"

    val svec_getvalue      : array_ -> int -> int -> vector
          = app3 "svec_getvalue"

    val svec_setvalue      : array_ -> int -> vector -> int
          = app3 "svec_setvalue"

    prim_val vector_  : int -> vector                 = 1 "create_string";
    prim_val lengthv_ : vector -> int                 = 1 "string_length";

    prim_val blitva__  : vector -> int -> Dynlib.cptr -> int -> int -> unit 
                                                      = 5 "blit_string";

    prim_val blitav__  : Dynlib.cptr -> int -> vector -> int -> int -> unit 
                                                      = 5 "blit_string";

    val blitva_  : vector -> int -> array_ -> int -> int -> unit 
           = fn v => fn i => fn a => blitva__ v i (svec_getbuffercptr a)

    val blitav_  : array_ -> int -> vector -> int -> int -> unit 
           = fn a => blitav__ (svec_getbuffercptr a)

    prim_val sub__     : Dynlib.cptr -> int -> elem         = 2 "get_nth_char";

    prim_val update__  : Dynlib.cptr -> int -> elem -> unit = 3 "set_nth_char";

    prim_val fill__    : Dynlib.cptr -> int -> int -> elem -> unit 
                                                            = 4 "fill_string";

    prim_val length__  : Dynlib.cptr -> int                 = 1 "string_length";

    prim_val blitaa__  : Dynlib.cptr -> int -> Dynlib.cptr -> int -> int -> unit 
                                                            = 5 "blit_string";

    val sub_     : array_ -> int -> elem
           = fn a => sub__ (svec_getbuffercptr a)

    val update_  : array_ -> int -> elem -> unit
           = fn a => update__ (svec_getbuffercptr a)

    val length_  : array_ -> int
           = fn a => length__ (svec_getbuffercptr a)

    val fill_    : array_ -> int -> int -> elem -> unit 
           = fn a => fill__ (svec_getbuffercptr a)

    val blitaa_  : array_ -> int -> array_ -> int -> int -> unit 
           = fn a => fn i => fn b => blitaa__ (svec_getbuffercptr a) i (svec_getbuffercptr b)

    val WORDSIZE : word = app1 "jit_get_wordsize" ()
in 

(* From here on, the code is identical with that in
   mosmllib/Word8Array.sml *)

type array = array_ ref;

val BYTESPERWORD = WORDSIZE div 0w8
val TAGBITS = 0w10
val maxLen = Word.toInt (((Word.<<(0w1,WORDSIZE - TAGBITS)) - 0w1) * BYTESPERWORD - 0w1)

(* Well, almost. Given this is the oly reason "mlp" is used here, it's
   a wonder no-body thought of this trick before.

   #if defined(msdos)
   #include "../config.dos/m.h"
   #elif defined(win32)
   #include "../config.w32/m.h"
   #else
   #include "../config/m.h"
   #endif

   #ifdef SIXTYFOUR
   val maxLen = 144115188075855863;  	(* = (2^54-1)*8-1, with 64 bit *)
   #else
   val maxLen = 16777211;  		(* = (2^22-1)*4-1, with 32 bit *)
   #endif
*)

val array0 = ref (array_ 0);

fun array(n, v: elem) =
    let val a = if n < 0 orelse n > maxLen then raise Size else array_ n 
    in fill_ a 0 n v; ref a end;

fun tabulate(n, f : int -> elem) =
  if n < 0 orelse n > maxLen then raise Size else
  let val a = array_ n
      fun init i = if i >= n then () else (update_ a i (f i); init (i+1))
  in init 0; ref a end;

fun fromList (vs : elem list) =
    let val n = List.length vs
	val a = if n > maxLen then raise Size else array_ n 
	fun init [] i = ()
	  | init (v::vs) i = (update_ a i v; init vs (i+1))
    in init vs 0; ref a end;

fun length (ref a) = length_ a;

fun sub(ref a, i) =
  if i < 0 orelse i >= length_ a then raise Subscript 
  else sub_ a i;

fun update(ref a, i, v) =
  if i < 0 orelse i >= length_ a then raise Subscript 
  else update_ a i v;

fun vector (ref a) =
    let val n = length_ a
	val newvec = vector_ n 
    in blitav_ a 0 newvec 0 n; newvec end;

fun copy {src = ref a1: array, dst = ref a2: array, di=i2} =
    let val n = length_ a1
    in
	if i2 < 0 orelse i2+n > length_ a2 then raise Subscript
	else blitaa_ a1 0 a2 i2 n
    end

fun copyVec {src = a1: vector, dst = ref a2: array, di=i2} =
    let val n = lengthv_ a1
    in
	if i2 < 0 orelse i2+n > length_ a2 then raise Subscript
	else blitva_ a1 0 a2 i2 n
    end

fun find (p : elem -> bool) (ref a) : elem option = 
    let val stop = length_ a
	fun lr j = 
	    if j < stop then 
		if p (sub_ a j) then SOME (sub_ a j) else lr (j+1)
	    else NONE
    in lr 0 end

fun exists (p : elem -> bool) (ref a) : bool = 
    let val stop = length_ a
	fun lr j = j < stop andalso (p (sub_ a j) orelse lr (j+1))
    in lr 0 end

fun all (p : elem -> bool) (ref a) : bool = 
    let val stop = length_ a
	fun lr j = j >= stop orelse (p (sub_ a j) andalso lr (j+1))
    in lr 0 end

fun foldl f e (ref a) = 
    let val stop = length_ a
	fun lr j res = if j < stop then lr (j+1) (f(sub_ a j, res))
		       else res
    in lr 0 e end

fun foldr f e (ref a) =
    let fun rl j res = if j >= 0 then rl (j-1) (f(sub_ a j, res))
		       else res
    in rl (length_ a - 1) e end

fun modify f (ref a) = 
    let val stop = length_ a
	fun lr j = if j < stop then (update_ a j (f(sub_ a j)); lr (j+1))
		   else ()
    in lr 0 end

fun app f (ref a) = 
    let val stop = length_ a
	fun lr j = if j < stop then (f(sub_ a j); lr (j+1))
		   else ()
    in lr 0 end

fun findi (p : int * elem -> bool) (ref a) : (int * elem) option = 
    let val stop = length_ a
	fun lr j = 
	    if j < stop then 
		if p (j, sub_ a j) then SOME (j, sub_ a j) else lr (j+1)
	    else NONE
    in lr 0 end

fun foldli f e (ref a) = 
    let val stop = length_ a 
	fun lr j res = 
	    if j < stop then lr (j+1) (f(j, sub_ a j, res))
	    else res
    in lr 0 e end;

fun foldri f e (ref a) = 
    let fun rl j res = 
	    if j >= 0 then rl (j-1) (f(j, sub_ a j, res))
	    else res
    in rl (length_ a - 1) e end;

fun modifyi f (ref a) = 
    let val stop = length_ a
	fun lr j = 
	    if j < stop then (update_ a j (f(j, sub_ a j)); lr (j+1))
	    else ()
    in lr 0 end;

fun appi f (ref a) = 
    let val stop = length_ a
	fun lr j = 
	    if j < stop then (f(j, sub_ a j); lr (j+1)) 
	    else ()
    in lr 0 end;

fun collate cmp (ref a1, ref a2) =
    let val n1 = length_ a1 
	and n2 = length_ a2
	val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point a1[0..j-1] = a2[0..j-1] *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
		case cmp(sub_ a1 j, sub_ a2 j) of
		    EQUAL => h (j+1)
		  | res   => res
    in h 0 end;
end
