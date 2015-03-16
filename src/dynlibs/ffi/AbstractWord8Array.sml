structure AbstractWord8Array =
 AbstractArray
  (type elem = word8
   type vector = GenericWord8Vector.vector)
    :> AbstractArray
      where type elem = word8
        and type vector = GenericWord8Vector.vector
