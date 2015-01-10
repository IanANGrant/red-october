(* WordPrim -- Word Primitives *)

signature WordPrim =
sig
eqtype word
type largeword

val wordSize   : int

val orb        : word * word -> word
val andb       : word * word -> word
val xorb       : word * word -> word
val notb       : word -> word
val ~          : word -> word

val <<         : word * Word.word -> word
val >>         : word * Word.word -> word
val ~>>        : word * Word.word -> word

val +          : word * word -> word
val -          : word * word -> word
val *          : word * word -> word
val div        : word * word -> word
val mod        : word * word -> word

val >          : word * word -> bool
val <          : word * word -> bool
val >=         : word * word -> bool
val <=         : word * word -> bool
val eq         : word * word -> bool

val toInt      : word -> int
val toIntX     : word -> int            (* with sign extension *)
val fromInt    : int -> word

val toLarge    : word -> largeword
val toLargeX   : word -> largeword      (* with sign extension *)
val fromLarge  : largeword -> word

val fromWord8ArraySlice : Word8ArraySlice.slice -> word
val toWord8ArraySlice : word -> Word8ArraySlice.slice

val toLargeWord   : word -> largeword
val toLargeWordX  : word -> largeword        (* with sign extension *)
val fromLargeWord : largeword -> word

val toLargeInt    : word -> int
val toLargeIntX   : word -> int         (* with sign extension *)
val fromLargeInt  : int -> word

val MAXPOS : word

end
