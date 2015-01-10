val _ = load "FifoBuffer";
val _ = load "GenericArray_sig";
val _ = load "GenericArraySlice_sig";
val _ = load "CharArray";
val _ = load "CharArraySlice";
val _ = load "WordReprSig";

signature TestFifo =
sig
  type T
  type slice
  type fifo
  val new : int -> slice
  val readwrite : fifo * T list ->  T list
  val write : fifo * T list ->  T list
end

functor TestFifo
  (type T
   val size : int
   structure ArrayStruct : GenericArray
   structure ArraySliceStruct : GenericArraySlice where type array = ArrayStruct.array
   structure Fifo : FifoBuffer where type slice = ArraySliceStruct.slice
   val conv  : T -> ArraySliceStruct.slice
   val iconv : ArraySliceStruct.slice -> T
   val zero : ArrayStruct.elem) :> TestFifo
       where type T = T
         and type slice = ArraySliceStruct.slice
         and type fifo = Fifo.fifo
=
struct
   type T = T
   type slice = ArraySliceStruct.slice
   type fifo = Fifo.fifo
   val new = fn n => ArraySliceStruct.full (ArrayStruct.array (n,zero))
   fun write (r,l) =
      List.foldl (fn (w,a) => (Fifo.write(r,conv w);(w::a))) [] l
   fun readwrite (r,l) = 
      let val buf = new size
      in List.foldl
           (fn (w,a) => (Fifo.resetCounters r;Fifo.write(r,conv w);Fifo.read (r,buf);(iconv buf)::a))
                  [] l
      end
end

functor SimpleTest
   (structure Fifo : FifoBuffer
    structure TestFifo : TestFifo
       where type fifo = Fifo.fifo
         and type T = string) =
struct
   local open TestFifo
      fun iterate f =
        let fun loop acc = 
          fn 0 => acc
           | n => loop (f acc) (n - 1)
        in loop
        end 
      val onetofive = ["one  ","two  ","three","four ","five "];
      val sixtoten  = ["six  ","seven","eight","nine ","ten  "];
      val r = Fifo.fifo 6
      val l = write (r,onetofive)
      val l = readwrite (r,sixtoten)
      val itfn = fn l => readwrite (r,l)
      val l = iterate itfn l 16
      val l1 = readwrite (r,l)
      val l2 = readwrite (r,l1)
      val l3 = readwrite (r,l2)
      val rc1 = Fifo.readCount r
      val wc1 = Fifo.writeCount r
      val sz1 = Fifo.size r
      val av1 = Fifo.avail r
      val () = Fifo.resetCounters r
      val rc2 = Fifo.readCount r
      val wc2 = Fifo.writeCount r
      val sz2 = Fifo.size r
      val av2 = Fifo.avail r
   in val result = ([l,l1,l2,l3],(rc1,wc1,sz1,av1),(rc2,wc2,sz2,av2))
   end
end

structure WordStruct
   :> GenericWord
        where type largeword = Word.word
          and type word = Word.word =
struct
   type largeword = Word.word
   open Word
end

structure Word8Struct
   :> GenericWord
        where type largeword = Word.word
          and type word = Word8.word =
struct
   type largeword = Word.word
   open Word8
end

structure Fifo =
 ConcreteFifoBuffer
    (structure WordStruct = WordStruct : GenericWord
     structure ArrayStruct = CharArray : GenericArray
     structure ArraySliceStruct = CharArraySlice : GenericArraySlice
     val length = CharVector.length
     val zero = #" ");

structure Fifo8 =
 ConcreteFifoBuffer
    (structure WordStruct = Word8Struct : GenericWord
     structure ArrayStruct = CharArray : GenericArray
     structure ArraySliceStruct = CharArraySlice : GenericArraySlice
     val length = CharVector.length
     val zero = #" ");

structure TR =
 TestFifo
  (type T = string
   val size = 5
   structure ArrayStruct = CharArray : GenericArray
   structure ArraySliceStruct = CharArraySlice : GenericArraySlice
   structure Fifo = Fifo : FifoBuffer
   val conv = CharArraySlice.full o CharArray.fromList o String.explode 
   val iconv = CharArraySlice.vector 
   val zero = #" ");

structure TR8 =
 TestFifo
  (type T = string
   val size = 5
   structure ArrayStruct = CharArray : GenericArray
   structure ArraySliceStruct = CharArraySlice : GenericArraySlice
   structure Fifo = Fifo8 : FifoBuffer
   val conv = CharArraySlice.full o CharArray.fromList o String.explode 
   val iconv = CharArraySlice.vector 
   val zero = #" ");

structure SimpleTest =
  SimpleTest
   (structure Fifo = Fifo : FifoBuffer
    structure TestFifo = TR : TestFifo);

structure SimpleTest8 =
  SimpleTest
   (structure Fifo = Fifo8 : FifoBuffer
    structure TestFifo = TR8 : TestFifo);

val res31 = SimpleTest.result;
val res8 = SimpleTest8.result;


