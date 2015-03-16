(* GenericWordVectorRepr *)

signature GenericWordVectorRepr =
sig
   include GenericWordVector
   type repr
   val fromVector : repr -> vector
end
