(* This should be abstract enough that one can define it for any
   general purpose programming language such as scheme, OCaml,
   JavaScript etc.*)

signature Values =
sig
   type valrep
   datatype tag =
           Double
         | Word
         | Address
         | Closure
         | Bytes
         | Abstract
         | Finalized
         | Weak
         | Ref
         | Atom of int
         | Tuple of int
   exception Repr
   val length : 'a -> int
   val repr : 'a -> valrep
   val value : valrep -> 'a
   val tag : 'a -> tag
   val sub : 'a * int -> valrep
   val update : 'a * int * 'b -> unit
   val new : tag * int -> valrep
   val dumpb : 'a -> unit
   val dumpw : 'a -> unit
   val cptr : 'a -> Dynlib.cptr
end
