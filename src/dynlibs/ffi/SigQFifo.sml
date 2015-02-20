(* A SigQFifo is a Fifo like SplitFiFo, with separate read/write
   pointers. But which uses signals for updating the buffer
   pointers. It also uses double-mapped buffers so that reads and
   writes can be done in one atomic operation even when they wrap. *)

signature SigQFifo =
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
   val writeSlice : (fifo * slice) -> unit
end

functor SigQFifo
   (structure WordStruct : GenericWord where type largeword = Word.word
    structure ArrayStruct : GenericArray
    structure ArraySliceStruct : GenericArraySlice
         where type elem = ArrayStruct.elem
           and type array = ArrayStruct.array
    val block_on_read : int -> unit
    val block_on_write : int -> unit
    val signal_read : int -> unit
    val signal_write : int -> unit
    val length : ArrayStruct.vector -> int
    val appi : (int * ArrayStruct.elem -> unit) -> ArrayStruct.vector -> unit
    val get_cptr : ArrayStruct.array -> Dynlib.cptr
    val zero : ArrayStruct.elem)
  :> SigQFifo
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
          val nbits =
             let fun loop n 0w0 = n
                   | loop n w = loop (n+1) (Word.>>(w,0w1))
             in loop 0 end
          fun op := (r,v) = MappedNativeWordRegister.:= (r,WordStruct.toLarge v)
          fun op ! r = WordStruct.fromLarge (MappedNativeWordRegister.! r)
          infix 2 :=
          val nought = WordStruct.fromInt 0
          val one = WordStruct.fromInt 1
          val int = WordStruct.toInt
          val word = WordStruct.fromInt
          val pagebits = nbits (MMap.PAGE_SIZE - 0w1)
          fun fifo (n,(readr,writer)) =
               if n < pagebits orelse n >= WordStruct.wordSize
                  then raise Size
                  else let val size' = one :<< (Word.fromInt (n - pagebits))
                           val slice = ArraySliceStruct.full (ArrayStruct.array (int size',zero))
                           val size = one :<< (Word.fromInt n)
                           val _ = (readr := nought; writer := nought)
                       in {buf = slice, size = size,
                           read = readr,
                           write = writer}
                       end
          fun fromSlice (slice,(readr,writer)) = 
             let val n = nbits (Word.fromInt (ArraySliceStruct.length slice - 1))
             in if n < pagebits orelse n >= WordStruct.wordSize
                   then raise Size
                   else let val size' = one :<< (Word.fromInt (n-1))
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
          val read =
            fn (r,s) =>
              let val len = ArraySliceStruct.length s
                  val _ = if len > avail r
                             then block_on_read len
                             else ()
                  val readr = #read r
                  val read' = !readr
                  val mask = #size r :- one
                  val buf = #buf r
                  val sub = fn (i,_) =>
                        ArraySliceStruct.sub
                          (buf, int ((read' :+ (word i)) & mask))
              in ArraySliceStruct.modifyi sub s;
                 readr := read' :+ (word len);
                 signal_read (int (!readr))
              end
          fun write (r,s) = (* Use MappedWord8ArraySlice.copyVec.
                               This may be a bug, but it has the
                               type {di : int, dst : array, src : slice} -> unit  *)
              let val len = ArraySliceStruct.length s
                  val _ = if len > free r
                             then block_on_write len
                             else ()
                  val writer = #write r
                  val write' = !writer
                  val mask = #size r :- one
                  val buf = #buf r
                  fun upd (i,w) =
                        ArraySliceStruct.update
                          (buf,int ((write' :+ (word i)) & mask), w)
              in ArraySliceStruct.appi upd s;
                 writer := write' :+ (word len);
                 signal_write (int (!writer))
              end
          val skipRead =
            fn (r,len) =>
                      let val _ = if len > avail r
                                     then block_on_read len
                                     else ()
                          val readr = #read r
                          val read' = !readr
                      in readr := read' :+ (word len);
                         signal_read (int (!readr))
                      end
          fun skipWrite (r,len) =
                      let val _ = if len > free r
                                     then block_on_write len
                                     else ()
                          val writer = #write r
                          val write' = !writer
                      in writer := write' :+ (word len);
                         signal_write (int (!writer))
                      end
          val readVec = (* Use ArraySliceStruct.vector *)
            fn (r,len) =>
              let val _ = if len > avail r
                             then block_on_read len
                             else ()
                  val readr = #read r
                  val read' = !readr
                  val mask = #size r :- one
                  val buf = #buf r 
                  val tab = fn i =>
                        ArraySliceStruct.sub
                          (buf, int ((read' :+ (word i)) & mask))
              in ArrayStruct.vector (ArrayStruct.tabulate (len,tab))
                 before (readr := read' :+ (word len);
                        signal_read (int (!readr)))
              end
          fun writeVec (r,v) = (* Use ArrayStruct.copyVec *)
              let val len = length v
                  val _ = if len > free r
                             then block_on_write len
                             else ()
                  val writer = #write r
                  val write' = !writer
                  val mask = #size r :- one
                  val buf = #buf r
                  fun upd (i,w) =
                        ArraySliceStruct.update
                          (buf,int ((write' :+ (word i)) & mask), w)
              in appi upd v;
                 writer := write' :+ (word len);
                 signal_write (int (!writer))
              end
          fun readByte r =
             let val _ = if avail r = 0
                            then block_on_read 1
                            else ()
                 val readr = #read r
                 val read' = !readr
                 val writer = #write r
                 val write' = !writer
                 val mask = #size r :- one
                 val buf = #buf r 
             in  ArraySliceStruct.sub (buf, int (read' & mask))
                 before (readr := (read' :+ one);
                         signal_read (int (!readr)))
             end
          fun writeByte (r,b) =
             let val _ = if free r = 0
                            then block_on_write 1
                            else ()
                 val writer = #write r
                 val write' = !writer
                 val mask = #size r :- one
                 val buf = #buf r 
             in ArraySliceStruct.update (buf, int (write' & mask),b)
                before (writer := write' :+ one;
                        signal_write (int (!writer)))
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
         val writeSlice : (fifo * slice) -> unit
            = write
      end
   end
end
