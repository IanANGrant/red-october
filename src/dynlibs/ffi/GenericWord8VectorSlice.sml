structure GenericWord8VectorSlice =
 GenericWordVectorSlice
  (structure WordVector = GenericWord8Vector
   structure AbstractVectorSlice = AbstractWord8VectorSlice)
     :> GenericWordVectorSlice
        where type elem = GenericWord8Vector.elem
          and type vector = GenericWord8Vector.vector
          and type slice = AbstractWord8VectorSlice.slice
