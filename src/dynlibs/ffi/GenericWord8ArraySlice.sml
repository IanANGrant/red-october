structure GenericWord8ArraySlice =
 GenericWordArraySlice
  (structure WordArray = GenericWord8Array
   structure AbstractArraySlice = AbstractWord8ArraySlice
   structure WordVector = GenericWord8Vector
   structure WordVectorSlice = GenericWord8VectorSlice)
 :> GenericWordArraySlice
      where type elem = GenericWord8Array.elem
        and type array = GenericWord8Array.array
        and type vector = GenericWord8Array.vector
        and type vector_slice = GenericWord8VectorSlice.slice
        and type slice = AbstractWord8ArraySlice.slice
