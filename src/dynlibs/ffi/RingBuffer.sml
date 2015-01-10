signature RingBuffer =
sig
   type ring
   type slice
   val ring : int -> ring
   val size : ring -> int
   val free : ring -> int
   val readCount : ring -> int
   val writeCount : ring -> int
   val resetCounters : ring -> unit
   val avail : ring -> int
   val read : (ring * slice) -> unit
   val write : (ring * slice) -> unit
end

functor ConcreteRingBuffer
   (structure WordStruct : GenericWord
    structure ArrayStruct : GenericArray
    structure ArraySliceStruct : GenericArraySlice
         where type elem = ArrayStruct.elem
           and type array = ArrayStruct.array
    val zero : ArrayStruct.elem)
  :> RingBuffer
     where type slice = ArraySliceStruct.slice =
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
      type ring = {buf  : ArraySliceStruct.slice, size : WordStruct.word,
                   read : WordStruct.word ref,   write : WordStruct.word ref}
      type slice = ArraySliceStruct.slice
      local
          val nought = WordStruct.fromInt 0
          val one = WordStruct.fromInt 1
          val int = WordStruct.toInt
          val word = WordStruct.fromInt
          fun ring n = 
               if n < 1 orelse n >= WordStruct.wordSize
                  then raise Size
                  else let val size' = one :<< (Word.fromInt n)
                           val array = ArrayStruct.array (int size',zero)
                       in {buf = (ArraySliceStruct.full array), size = size',
                           read = ref nought,
                           write = ref nought}
                       end
          fun avail r = int (!(#write r) :- (!(#read r)))
          fun size r = int (#size r)
          fun free r = size r - (avail r)
          fun readCount r = int (!(#read r))
          fun writeCount r = int (!(#write r))
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
      in
         val ring : int -> ring
            = ring
         val free : ring -> int
            = free
         val avail : ring -> int
            = avail
         val readCount : ring -> int
            = readCount
         val writeCount : ring -> int
            = writeCount
         val resetCounters : ring -> unit
            = resetCounters
         val read : (ring * slice) -> unit
            = read
         val write : (ring * slice) -> unit
            = write
         val size : ring -> int
            = size
      end
   end
end
      
