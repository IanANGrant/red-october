structure MappedFifo =
struct
   local
      fun mkRegisterVector n slc =
         let open MappedNativeRegister
             open MappedWord8ArraySlice
             val rl = (Word.wordSize + 7) div 8
         in if length slc < rl 
               then raise Fail "MMapFifo: register slice too short"
               else Vector.tabulate (n,fn i => new (subslice(slc,i*4,SOME rl)))
         end
   in structure Fifo =
        SplitFifo
          (structure WordStruct =
           struct
              type largeword = Word.word
              open Word
           end
           structure ArrayStruct = MappedWord8Array
           structure ArraySliceStruct = MappedWord8ArraySlice
           val length = Word8Vector.length
           val appi = Word8Vector.appi
           val get_cptr = MappedWord8Array.get_cptr
           val zero = 0w0)
     fun create (slc,regsslc) =
        let val nregs = 2
            val wbytes = (Word.wordSize + 7) div 8
            val regsv = mkRegisterVector 2 (regsslc)
            val regs = (Vector.sub (regsv,0),Vector.sub (regsv,1))
            val buff = Fifo.fromSlice (slc,regs)
         in buff
         end
   end
end
