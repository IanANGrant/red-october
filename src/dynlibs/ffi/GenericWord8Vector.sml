structure GenericWord8Vector =
 GenericWordVector
  (structure AbstractVector = AbstractWord8Vector
   structure WordVector = Word8Vector)
    :> GenericWordVectorRepr
          where type vector = AbstractWord8Vector.vector
            and type repr = Word8Vector.vector
            and type elem = AbstractWord8Vector.elem
