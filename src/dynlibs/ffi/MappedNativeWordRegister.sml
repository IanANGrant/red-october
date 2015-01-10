structure MappedNativeWordRegister :> MappedNativeWordRegister =
struct
   type register = MappedNativeRegister.register
   val new = MappedNativeRegister.new
   fun op := (reg,w) =
     let val v = ValRepr.cptrWord8Vector (ValRepr.longFromWord w)
     in MappedNativeRegister.:= (reg,v)
     end
   fun op ! reg =
      ValRepr.wordFromLong (ValRepr.word8VectorCptr (MappedNativeRegister.! reg))
end
