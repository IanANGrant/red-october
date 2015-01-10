(* GenericWord8ArraySlice -- SML Basis Library *)

signature GenericArraySlice =
sig
   type elem
   type array
   type vector
   type vector_slice
   
   type slice
   
   val length   : slice -> int
   val sub      : slice * int -> elem
   val update   : slice * int * elem  -> unit
   val slice    : array * int * int option -> slice
   val full     : array -> slice
   val subslice : slice * int * int option -> slice
   val base     : slice -> array * int * int
   val vector   : slice -> vector
   val copy     : {src: slice, dst: array, di: int} -> unit
   val copyVec  : {src: vector_slice, dst: array, di: int} -> unit 
   val isEmpty  : slice -> bool
   val getItem  : slice -> (elem * slice) option
   
   val find     : (elem -> bool) -> slice -> elem option
   val exists   : (elem -> bool) -> slice -> bool
   val all      : (elem -> bool) -> slice -> bool
   
   val app      : (elem -> unit) -> slice -> unit
   val foldl    : (elem * 'b -> 'b) -> 'b -> slice -> 'b
   val foldr    : (elem * 'b -> 'b) -> 'b -> slice -> 'b
   val modify   : (elem -> elem) -> slice -> unit
   
   val findi    : (int * elem -> bool) -> slice -> (int * elem) option
   val appi     : (int * elem -> unit) -> slice -> unit
   val foldli   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
   val foldri   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
   val modifyi  : (int * elem -> elem) -> slice -> unit
   
   val collate  : (elem * elem -> order) -> slice * slice -> order
end
