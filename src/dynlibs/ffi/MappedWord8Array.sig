   eqtype array
   type elem   = Word8.word
   type vector = Word8Vector.vector

   val maxLen   : int

   val alloc_final : Dynlib.cptr * Int.int * Dynlib.cptr * Int.int -> array

   val malloc : Int.int -> Dynlib.cptr
   val free : Dynlib.cptr -> unit

   val get_cptr : array -> Dynlib.cptr

val array    : int * elem -> array
val tabulate : int * (int -> elem) -> array
val fromList : elem list -> array 

val length   : array -> int
val sub      : array * int -> elem
val update   : array * int * elem -> unit
val vector   : array -> vector

val copy     : {src: array,  dst: array, di: int} -> unit
val copyVec  : {src: vector, dst: array, di: int} -> unit

val find     : (elem -> bool) -> array -> elem option
val exists   : (elem -> bool) -> array -> bool
val all      : (elem -> bool) -> array -> bool

val app      : (elem -> unit) -> array -> unit
val foldl    : (elem * 'b -> 'b) -> 'b -> array -> 'b
val foldr    : (elem * 'b -> 'b) -> 'b -> array -> 'b
val modify   : (elem -> elem) -> array -> unit

val findi    : (int * elem -> bool) -> array -> (int * elem) option
val appi     : (int * elem -> unit) -> array -> unit
val foldli   : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
val foldri   : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
val modifyi  : (int * elem -> elem) -> array -> unit

val collate  : (elem * elem -> order) -> array * array -> order

