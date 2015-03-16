structure AbstractWord8Vector =
 AbstractVector
  (type elem = word8)
    :> AbstractVector
      where type elem = word8
