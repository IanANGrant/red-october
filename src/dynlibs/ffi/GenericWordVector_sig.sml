(* GenericWordVector -- SML Basis Library *)

signature GenericWordVector =
sig
   type vector
   type elem

   val maxLen   : int

   val fromList : elem list -> vector
   val tabulate : int * (int -> elem) -> vector

   val length   : vector -> int
   val sub      : vector * int -> elem
   val update   : vector * int * elem -> vector
   val concat   : vector list -> vector

   val find     : (elem -> bool) -> vector -> elem option
   val exists   : (elem -> bool) -> vector -> bool
   val all      : (elem -> bool) -> vector -> bool

   val app      : (elem -> unit) -> vector -> unit
   val map      : (elem -> elem) -> vector -> vector
   val foldl    : (elem * 'b -> 'b) -> 'b -> vector -> 'b
   val foldr    : (elem * 'b -> 'b) -> 'b -> vector -> 'b

   val findi    : (int * elem -> bool) -> vector -> (int * elem) option
   val appi     : (int * elem -> unit) -> vector -> unit
   val mapi     : (int * elem -> elem) -> vector -> vector
   val foldli   : (int * elem * 'b -> 'b) -> 'b -> vector -> 'b
   val foldri   : (int * elem * 'b -> 'b) -> 'b -> vector -> 'b

   val collate  : (elem * elem -> order) -> vector * vector -> order
end

(* [vector] is the type of one-dimensional, immutable, zero-based
   constant-time-access vectors with elements of type elem, that is,
   words of some kind.  Type vector admits equality, and vectors v1
   and v2 are equal if they have the same length and their elements
   are equal.

   All operations are as for Vector.vector.
*)
