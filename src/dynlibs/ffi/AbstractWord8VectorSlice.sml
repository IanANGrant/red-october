structure AbstractWord8VectorSlice =
 AbstractVectorSlice
  (type vector = GenericWord8Vector.vector)
    :> AbstractVectorSlice
      where type vector = GenericWord8Vector.vector
