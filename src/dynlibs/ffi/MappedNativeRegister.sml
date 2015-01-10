structure MappedNativeRegister :> MappedNativeRegister =
struct
   type register = MappedWord8ArraySlice.slice
   fun new slc = 
      if MappedWord8ArraySlice.length slc = (Word.wordSize + 7) div 8
         then slc 
         else raise Size
   fun op := (reg,v) =
     let val (arr,offs,len) = MappedWord8ArraySlice.base reg
     in MappedWord8Array.copyVec {di=offs, dst=arr, src=v}
     end
   fun op ! reg = MappedWord8ArraySlice.vector reg
end


