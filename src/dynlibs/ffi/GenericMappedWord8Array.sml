 structure GenericMappedWord8Array =
 GenericWordArray
  (structure AbstractArray = AbstractWord8Array
   structure WordVector = GenericWord8Vector
   structure WordArray = MappedWord8Array)
    :> GenericWordArrayRepr
          where type array = AbstractWord8Array.array
            and type vector = GenericWord8Vector.vector
            and type repr = MappedWord8Array.array
            and type elem = AbstractWord8Array.elem
