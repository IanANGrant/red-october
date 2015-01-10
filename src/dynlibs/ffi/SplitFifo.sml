(* A SplitFifo is a Fifo with separate read/write pointers. It's also
   split in the sense that with a simplex channel one process can read
   it and another can write to it independently (i.e. without any
   semaphores or spinlocks or anything.) *)

signature SplitFifo =
sig
   type elem
   type fifo
   type slice
   type array
   type vector
   val fifo : int * (MappedNativeWordRegister.register *
                     MappedNativeWordRegister.register) -> fifo
   val fromSlice : slice  * (MappedNativeWordRegister.register *
                             MappedNativeWordRegister.register) -> fifo
   val buffer : fifo -> slice
   val size : fifo -> int
   val free : fifo -> int
   val avail : fifo -> int
   val readCount : fifo -> int
   val writeCount : fifo -> int
   val resetCounters : fifo -> unit
   val zeroBuffer : fifo -> unit
   val bufferPtr : fifo -> Dynlib.cptr
   val readPtr : fifo -> Dynlib.cptr
   val writePtr : fifo -> Dynlib.cptr
   val read : (fifo * slice) -> unit
   val write : (fifo * slice) -> unit
   val readVec : (fifo * int) -> vector
   val writeVec : (fifo * vector) -> unit
   val skipRead : (fifo * int) -> unit
   val skipWrite : (fifo * int) -> unit
   val readByte : fifo -> elem
   val writeByte : fifo * elem -> unit
   val readSlice : (fifo * int) -> slice
   val writeSlice : (fifo * slice) -> unit
   val dump : fifo -> unit
end

functor SplitFifo
   (structure WordStruct : GenericWord where type largeword = Word.word
    structure ArrayStruct : GenericArray
    structure ArraySliceStruct : GenericArraySlice
         where type elem = ArrayStruct.elem
           and type array = ArrayStruct.array
    val length : ArrayStruct.vector -> int
    val appi : (int * ArrayStruct.elem -> unit) -> ArrayStruct.vector -> unit
    val get_cptr : ArrayStruct.array -> Dynlib.cptr
    val zero : ArrayStruct.elem)
  :> SplitFifo
     where type elem = ArrayStruct.elem
       and type slice = ArraySliceStruct.slice
       and type array = ArrayStruct.array
       and type vector = ArrayStruct.vector =
struct
   local
      infix 5 :-
      infix 5 :+
      infix 4 &
      infix 4 :<<
      val op :- = WordStruct.-
      val op :+ = WordStruct.+
      val op :<< = WordStruct.<<
      val op & = WordStruct.andb
   in
      type elem = ArrayStruct.elem
      type fifo = {buf  : ArraySliceStruct.slice, size : WordStruct.word,
                   read : MappedNativeWordRegister.register,
                   write : MappedNativeWordRegister.register}
      type slice = ArraySliceStruct.slice
      type array = ArrayStruct.array
      type vector = ArrayStruct.vector
      local 
          fun op := (r,v) = MappedNativeWordRegister.:= (r,WordStruct.toLarge v)
          fun op ! r = WordStruct.fromLarge (MappedNativeWordRegister.! r)
          infix 2 :=
          val nought = WordStruct.fromInt 0
          val one = WordStruct.fromInt 1
          val int = WordStruct.toInt
          val word = WordStruct.fromInt
          fun fifo (n,(readr,writer)) =
               if n < 1 orelse n >= WordStruct.wordSize
                  then raise Size
                  else let val size' = one :<< (Word.fromInt n)
                           val slice = ArraySliceStruct.full (ArrayStruct.array (int size',zero))
                           val _ = (readr := nought;writer := nought)
                       in {buf = slice, size = size',
                           read = readr,
                           write = writer}
                       end
          fun fromSlice (slice,(readr,writer)) = 
             let fun nbits n 0w0 = n
                   | nbits n w = nbits (n+1) (Word.>>(w,0w1))
                 val n = nbits 0 (Word.fromInt (ArraySliceStruct.length slice - 1))
             in if n < 1 orelse n >= WordStruct.wordSize
                   then raise Size
                   else let val size' = one :<< (Word.fromInt n)
                            val _ = (readr := nought;writer := nought)
                        in {buf = slice, size = size',
                            read = readr,
                            write = writer}
                        end
             end
          fun buffer r = #buf r
          fun avail r = int (!(#write r) :- (!(#read r)))
          fun size r = int (#size r)
          fun free r = size r - (avail r)
          fun readCount r = int (!(#read r))
          fun writeCount r = int (!(#write r))
          val bufferPtr =
            fn b =>
              let val (arr,offs,_) = ArraySliceStruct.base (#buf b)
              in ValRepr.cptr_offs (get_cptr arr) offs
              end
          val writePtr =
            fn b =>
              let val (arr,offs,_) = ArraySliceStruct.base (#buf b)
              in ValRepr.cptr_offs (get_cptr arr) ((offs+(writeCount b)) mod (size b))
              end
          val readPtr =
            fn b =>
              let val (arr,offs,_) = ArraySliceStruct.base (#buf b)
              in ValRepr.cptr_offs (get_cptr arr)  ((offs+(readCount b)) mod (size b))
              end
          fun zeroBuffer r = ((#write r) := nought;
                              (#read r) := nought)
          fun resetCounters r =
               let val wc = !(#write r)
                   val rc = !(#read r)
                   val avail = wc :- rc
                   val modulo = fn w => w & (#size r :- one)
                   val read' = modulo rc
                   val write' = read' :+ avail
               in (#write r) := write';
                  (#read r) := read'
               end
          val read =
            fn (r,s) =>
              let val len = ArraySliceStruct.length s
              in if len > avail r
                    then raise Size
                    else 
                      let val readr = #read r
                          val read' = !readr
                          val mask = #size r :- one
                          val buf = #buf r 
                          val sub = fn (i,_) =>
                                ArraySliceStruct.sub
                                  (buf, int ((read' :+ (word i)) & mask))
                      in ArraySliceStruct.modifyi sub s;
                         readr := read' :+ (word len)
                      end
              end
          fun write (r,s) =
              let val len = ArraySliceStruct.length s
              in if len > free r
                 then raise Size
                 else let val writer = #write r
                          val write' = !writer
                          val mask = #size r :- one
                          val buf = #buf r
                          fun upd (i,w) =
                                ArraySliceStruct.update
                                  (buf,int ((write' :+ (word i)) & mask), w)
                      in ArraySliceStruct.appi upd s;
                         writer := write' :+ (word len)
                      end
              end
          val skipRead =
            fn (r,len) =>
                 if len > avail r
                    then raise Size
                    else 
                      let val readr = #read r
                          val read' = !readr
                      in readr := read' :+ (word len)
                      end
          fun skipWrite (r,len) =
              if len > free r
                 then raise Size
                 else let val writer = #write r
                          val write' = !writer
                      in writer := write' :+ (word len)
                      end
          fun readSlice (r,n) =
             let val readr = #read r
                 val read' = !readr
                 val mask = #size r :- one
                 val buf = #buf r
                 val offs = int ((read') & mask)
             in if n <= avail r andalso offs + n < size r
                   then ArraySliceStruct.subslice (buffer r,offs,SOME n)
                        before readr := read' :+ (word n)
                   else raise Size
             end
          val readVec =
            fn (r,len) =>
              let 
              in if len > avail r
                    then raise Size
                    else 
                      let val readr = #read r
                          val read' = !readr
                          val mask = #size r :- one
                          val buf = #buf r 
                          val tab = fn i =>
                                ArraySliceStruct.sub
                                  (buf, int ((read' :+ (word i)) & mask))
                      in ArrayStruct.vector (ArrayStruct.tabulate (len,tab))
                         before readr := read' :+ (word len)
                      end
              end
          fun writeVec (r,v) =
              let val len = length v
              in if len > free r
                 then raise Size
                 else let val writer = #write r
                          val write' = !writer
                          val mask = #size r :- one
                          val buf = #buf r
                          fun upd (i,w) =
                                ArraySliceStruct.update
                                  (buf,int ((write' :+ (word i)) & mask), w)
                      in appi upd v;
                         writer := write' :+ (word len)
                      end
              end
          fun readByte r =
             let val readr = #read r
                 val read' = !readr
                 val writer = #write r
                 val write' = !writer
                 val mask = #size r :- one
                 val buf = #buf r 
             in if 1 <= avail r
                   then ArraySliceStruct.sub (buf, int (read' & mask))
                         before readr := (read' :+ one)
                   else raise Size
             end
          fun writeByte (r,b) =
              if free r = 0
                 then raise Size
                 else let val writer = #write r
                          val write' = !writer
                          val mask = #size r :- one
                          val buf = #buf r 
                      in ArraySliceStruct.update (buf, int (write' & mask),b)
                         before writer := write' :+ one
                      end
         fun dump buff =
            let val readptr = readPtr buff
                val len =  avail buff
                val readc = readCount buff
                val size = size buff
                val readoff = readc mod size
            in if (readoff + len) > size
                  then let val (s1,l1) = (readptr,(size - readoff))
                           val (s2,l2) = (bufferPtr buff, ((readoff + len) - size))
                       in MemDebug.dumpb_offs s1 0 l1;
                          print ("==wrapped==\n");
                          MemDebug.dumpb_offs s2 0 l2
                        end
                  else  MemDebug.dumpb_offs readptr 0 len
            end
      in
         val fifo : int * (MappedNativeWordRegister.register *
                           MappedNativeWordRegister.register) -> fifo
            = fifo
         val fromSlice : slice * (MappedNativeWordRegister.register *
                                  MappedNativeWordRegister.register) -> fifo
            = fromSlice
         val buffer : fifo -> slice
            = buffer
         val size : fifo -> int
            = size
         val free : fifo -> int
            = free
         val avail : fifo -> int
            = avail
         val readCount : fifo -> int
            = readCount
         val writeCount : fifo -> int
            = writeCount
         val bufferPtr : fifo -> Dynlib.cptr
            = bufferPtr
         val readPtr : fifo -> Dynlib.cptr
            = readPtr
         val writePtr : fifo -> Dynlib.cptr
            = writePtr
         val resetCounters : fifo -> unit
            = resetCounters
         val zeroBuffer : fifo -> unit
            = zeroBuffer
         val read : (fifo * slice) -> unit
            = read
         val write : (fifo * slice) -> unit
            = write
         val skipRead : (fifo * int) -> unit
            = skipRead
         val skipWrite : (fifo * int) -> unit
            = skipWrite
         val readVec : (fifo * int) -> vector
            = readVec
         val writeVec : (fifo * vector) -> unit
            = writeVec
         val readByte : fifo -> elem
            = readByte
         val writeByte : fifo * elem -> unit
            = writeByte
         val readSlice : (fifo * int) -> slice
            = readSlice
         val writeSlice : (fifo * slice) -> unit
            = write
         val dump : fifo -> unit
            = dump
      end
   end
end
