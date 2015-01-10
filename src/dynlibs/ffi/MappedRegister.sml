structure MappedRegister :> MappedRegister =
struct
   type register = MappedWord8ArraySlice.slice
   fun new slc = 
      if MappedWord8ArraySlice.length slc = (Word.wordSize + 7) div 8
         then slc 
         else raise Size
   fun op := (reg,w) =
     let val (arr,offs,len) = MappedWord8ArraySlice.base reg
         val v = ValRepr.wordWord8Vector w
     in MappedWord8Array.copyVec {di=offs, dst=arr, src=v}
     end
   fun op ! reg =
      ValRepr.word8VectorWord (MappedWord8ArraySlice.vector reg)
end


