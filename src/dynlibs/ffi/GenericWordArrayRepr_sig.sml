(* GenericWordArrayRepr *)

signature GenericWordArrayRepr =
sig
   include GenericWordArray
   type repr
   val fromArray : repr -> array
end
