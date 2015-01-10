structure MappedNativeAddressRegister :> MappedNativeAddressRegister =
struct
   type register = MappedNativeRegister.register
   val new = MappedNativeRegister.new
   fun op := (reg,addr) =
     let val v = ValRepr.cptrWord8Vector addr
     in MappedNativeRegister.:= (reg,v)
     end
   fun op ! reg =
      ValRepr.word8VectorCptr (MappedNativeRegister.! reg)
end


