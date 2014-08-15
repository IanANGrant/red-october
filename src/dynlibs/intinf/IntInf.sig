(* LargeInt -- arbitrary-precision integers 1995-09-04, 1998-04-12 2014-07-17 *)
(* This module requires Dynlib and the GNU GMP package to be installed *)

type int

val precision : int option
val minInt    : int option
val maxInt    : int option

val ~    : int -> int
val +    : int * int -> int
val -    : int * int -> int
val *    : int * int -> int
val div  : int * int -> int
val mod  : int * int -> int
val quot : int * int -> int
val rem  : int * int -> int
val <    : int * int -> bool
val >    : int * int -> bool
val <=   : int * int -> bool
val >=   : int * int -> bool
val eq   : int * int -> bool
val ne   : int * int -> bool
val abs  : int -> int
val min  : int * int -> int
val max  : int * int -> int

val gcd    : int * int -> int
val lcm    : int * int -> int
val invert : int * int -> int
val sqrt : int -> int
val sqrtrem : int -> int * int

val op << : int * Int.int -> int
val op >> : int * Int.int -> int
val op ~>> : int * Int.int -> int
val andb : int * int -> int
val orb : int * int -> int
val xorb : int * int -> int
val notb : int -> int

val msb : int -> Int.int

val init2  : Int.int * Int.int -> int
val combit : int * Int.int-> unit
val clrbit : int * Int.int-> unit
val setbit : int * Int.int-> unit

val tstbit : int * Int.int-> bool
val popcount : int -> int option
val hamdist : int * int -> int option
val scan0 : int * Int.int-> int option
val scan1 : int * Int.int-> int option

val divMod   : int * int -> int * int
val quotRem  : int * int -> int * int
val pow      : int * Int.int -> int
val log2     : int -> Int.int

val sign     : int -> Int.int
val sameSign : int * int -> bool
val compare  : int * int -> order

val getCptr : int -> Dynlib.cptr

val fromInt    : Int.int -> int
val toInt      : int -> Int.int		(* Overflow *)
val toLarge    : int -> int
val fromLarge  : int -> int

val fromString : string -> int option
val toString   : int -> string

val import : {endian : Int.int, nails : Int.int, order : Int.int, size : Int.int} -> 
              Word8ArraySlice.slice * Int.int * Int.int -> int

val export : {endian : Int.int, nails : Int.int, order : Int.int, size : Int.int} -> 
              int -> Word8ArraySlice.slice * Int.int * Int.int

val rawformat : {endian : Int.int, nails : Int.int, order : Int.int, size : Int.int}

val scan : StringCvt.radix
           -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
val fmt  : StringCvt.radix -> int -> string
