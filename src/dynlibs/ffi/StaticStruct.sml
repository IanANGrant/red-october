signature StaticStruct =
sig
   include FifoBuffer
   type cptr
   val bufPtr : fifo -> cptr
   val writePtr : fifo -> cptr
   val writeFloat : (fifo * real) -> unit
   val writeDouble : (fifo * real) -> unit
   val writeWord32 : (fifo * Word.word) -> unit
   val writeWord16 : (fifo * Word.word) -> unit
   val writeWord8 : (fifo * Word.word) -> unit
(*   val writeInt16 : (fifo * int) -> unit
   val writeInt8 : (fifo * int) -> unit
   val writeString : (fifo * string) -> unit *)
   val writeAddr : (fifo * cptr) -> unit
   val readPtr : fifo -> cptr
   val readFloat : fifo -> real
   val readDouble : fifo -> real
   val readWord32 : fifo -> Word.word
   val readWord16 : fifo -> Word.word
   val readWord8 : fifo -> Word.word
(*   val readInt16 : fifo -> int
   val readInt8 : fifo -> int
   val readString : fifo -> string *)
   val readAddr : fifo -> cptr
end

structure StaticStruct :> StaticStruct 
   where type array = StaticWord8Array.array
     and type slice = StaticWord8ArraySlice.slice
     and type vector = Word8Vector.vector
     and type cptr = Dynlib.cptr =
struct
   local
      structure WordStruct
         :> GenericWord
              where type largeword = Word.word
                and type word = Word.word =
      struct
         type largeword = Word.word
         open Word
      end
      structure Fifo =
        ConcreteFifoBuffer
           (structure WordStruct = WordStruct : GenericWord 
            structure ArrayStruct = StaticWord8Array : GenericArray
            structure ArraySliceStruct = StaticWord8ArraySlice : GenericArraySlice
            val length = Word8Vector.length
            val zero = 0w0)
      type buffer = Fifo.fifo
   in open Fifo
      type cptr = Dynlib.cptr
      val bufPtr =
            fn b =>
              let val (arr,offs,len) = StaticWord8ArraySlice.base (Fifo.buffer b)
              in StaticBuffer.svec_cptr_byte_offset
                    (StaticBuffer.buffer arr) offs
              end
      val writePtr =
            fn b =>
              let val (arr,offs,len) = StaticWord8ArraySlice.base (Fifo.buffer b)
              in StaticBuffer.svec_cptr_byte_offset
                    (StaticBuffer.buffer arr) (offs+(Fifo.writeCount b))
              end
      val readPtr =
            fn b =>
              let val (arr,offs,len) = StaticWord8ArraySlice.base (Fifo.buffer b)
              in StaticBuffer.svec_cptr_byte_offset
                    (StaticBuffer.buffer arr) (offs+(Fifo.readCount b))
              end
      fun writeFloat (b,x) =
          Fifo.writeVec (b,RealRepr.floatVec x)
      fun readFloat b =
          RealRepr.vecFloat (Fifo.readVec (b,4))
      fun writeDouble (b,x) =
          Fifo.writeVec (b,RealRepr.doubleVec x)
      fun readDouble b =
          RealRepr.vecDouble (Fifo.readVec (b,8))
      fun writeWord32 (b,w) =
          Fifo.writeVec (b,WordRepr.word32Vec w)
      fun readWord32 b =
          WordRepr.vecWord32 (Fifo.readVec (b,4))
      fun writeWord16 (b,w) =
          Fifo.writeVec (b,WordRepr.word16Vec w)
      fun readWord16 b =
          WordRepr.vecWord16 (Fifo.readVec (b,2))
      fun writeWord8 (b,w) =
          Fifo.writeVec (b,WordRepr.word8Vec w)
      fun readWord8 b =
          WordRepr.vecWord8 (Fifo.readVec (b,1))
      fun writeAddr (b,w) =
          Fifo.writeVec (b, StaticBuffer.svec_getcptrvalue w)
      fun readAddr b =
          StaticBuffer.svec_setcptrvalue (Fifo.readVec (b,4))
   end
end
