structure GenericMappedWord8ArraySlice =
  GenericWordArraySlice
   (structure WordArray = GenericMappedWord8Array
    structure AbstractArraySlice = AbstractWord8ArraySlice
    structure WordVector = GenericWord8Vector
    structure WordVectorSlice = GenericWord8VectorSlice)
 :> GenericWordArraySlice
      where type elem = GenericMappedWord8Array.elem
        and type array = GenericMappedWord8Array.array
        and type vector = GenericMappedWord8Array.vector
        and type vector_slice = GenericWord8VectorSlice.slice
        and type slice = AbstractWord8ArraySlice.slice
