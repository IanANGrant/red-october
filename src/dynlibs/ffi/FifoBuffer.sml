signature FifoBuffer =
sig
   type elem
   type fifo
   type slice
   type array
   type vector
   val fifo : int -> fifo
   val fromSlice : slice -> fifo
   val buffer : fifo -> slice
   val size : fifo -> int
   val free : fifo -> int
   val avail : fifo -> int
   val readCount : fifo -> int
   val writeCount : fifo -> int
   val resetCounters : fifo -> unit
   val zeroBuffer : fifo -> unit
   val read : (fifo * slice) -> unit
   val write : (fifo * slice) -> unit
   val readVec : (fifo * int) -> vector
   val writeVec : (fifo * vector) -> unit
   val skipRead : (fifo * int) -> unit
   val skipWrite : (fifo * int) -> unit
   val readSlice : (fifo * int) -> slice
   val readByte : fifo -> elem
   val writeByte : fifo * elem -> unit
   val writeSlice : (fifo * slice) -> unit
end

functor ConcreteFifoBuffer
   (structure WordStruct : GenericWord
    structure ArrayStruct : GenericArray
    structure ArraySliceStruct : GenericArraySlice
         where type elem = ArrayStruct.elem
           and type array = ArrayStruct.array
    val length : ArrayStruct.vector -> int
    val zero : ArrayStruct.elem)
  :> FifoBuffer
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
                   read : WordStruct.word ref,   write : WordStruct.word ref}
      type slice = ArraySliceStruct.slice
      type array = ArrayStruct.array
      type vector = ArrayStruct.vector
      local
          val nought = WordStruct.fromInt 0
          val one = WordStruct.fromInt 1
          val int = WordStruct.toInt
          val word = WordStruct.fromInt
          fun fifo n = 
               if n < 1 orelse n >= WordStruct.wordSize
                  then raise Size
                  else let val size' = one :<< (Word.fromInt n)
                           val slice = ArraySliceStruct.full (ArrayStruct.array (int size',zero))
                       in {buf = slice, size = size',
                           read = ref nought,
                           write = ref nought}
                       end
          fun fromSlice slice = 
             let fun nbits n 0w0 = n
                   | nbits n w = nbits (n+1) (Word.>>(w,0w1))
                 val n = nbits 0 (Word.fromInt (ArraySliceStruct.length slice - 1)) - 1
             in if n < 1 orelse n >= WordStruct.wordSize
                   then raise Size
                   else let val size' = one :<< (Word.fromInt n)
                        in {buf = slice, size = size',
                            read = ref nought,
                            write = ref nought}
                        end
             end
          fun buffer r = #buf r
          fun avail r = int (!(#write r) :- (!(#read r)))
          fun size r = int (#size r)
          fun free r = size r - (avail r)
          fun readCount r = int (!(#read r))
          fun writeCount r = int (!(#write r))
          fun zeroBuffer r = ((#write r) := nought;
                              (#read r) := nought)
          fun resetCounters r =
               let val wc = !(#write r)
                   val rc = !(#read r)
                   val avail = wc :- rc
                   val (read',write') =
                         if avail = nought
                            then (nought,nought)
                            else let val (arr,offs,len) = ArraySliceStruct.base (#buf r)
                                 in (ArraySliceStruct.copy
                                       {src = ArraySliceStruct.subslice
                                               (#buf r,int rc,SOME (int avail)),
                                        di = offs, dst = arr};
                                     (nought,avail))
                                 end
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
                          val buf = #buf r 
                          val sub = fn (i,_) =>
                                ArraySliceStruct.sub
                                  (buf, int (read' :+ (word i)))
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
                          val buf = #buf r
                          fun upd (i,w) =
                                ArraySliceStruct.update
                                  (buf,int (write' :+ (word i)), w)
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
          val readVec =
            fn (r,len) =>
              let 
              in if len > avail r
                    then raise Size
                    else 
                      let val readr = #read r
                          val read' = !readr
                          val buf = #buf r 
                          val tab = fn (i) =>
                                ArraySliceStruct.sub
                                  (buf, int (read' :+ (word i)))
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
                          val buf = #buf r
                          val (arr,offs,_) = ArraySliceStruct.base (#buf r)
                      in ArrayStruct.copyVec {dst = arr, di = offs + (int write'), src = v};
                         writer := write' :+ (word len)
                      end
              end
          fun readSlice (r,n) =
                if n <= avail r
                   then ArraySliceStruct.subslice (buffer r,int (!(#read r)),SOME n)
                        before (#read r) := !(#read r) :+ (word n)
                   else raise Size
          fun readByte r =
                if 1 <= avail r
                   then ArraySliceStruct.sub (buffer r,int (!(#read r)))
                        before (#read r) := !(#read r) :+ one
                   else raise Size
          fun writeByte (r,b) =
              if free r = 0
                 then raise Size
                 else let val writer = #write r
                          val write' = !writer
                          val buf = #buf r
                      in ArraySliceStruct.update(buf,(int write'),b);
                         writer := write' :+ one
                      end
      in
         val fifo : int -> fifo
            = fifo
         val fromSlice : slice -> fifo
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
         val readSlice : (fifo * int) -> slice
            = readSlice
         val readByte : fifo -> elem
            = readByte
         val writeByte : fifo * elem -> unit
            = writeByte
         val writeSlice : (fifo * slice) -> unit
            = write
      end
   end
end
