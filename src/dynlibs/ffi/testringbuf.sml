val _ = load "RingBuffer";
val _ = load "GenericArray_sig";
val _ = load "GenericArraySlice_sig";
val _ = load "CharArray";
val _ = load "CharArraySlice";
val _ = load "WordReprSig";

signature TestRing =
sig
  type T
  type slice
  type ring
  val new : int -> slice
  val readwrite : ring * T list ->  T list
  val write : ring * T list ->  T list
end

functor TestRing
  (type T
   val size : int
   structure ArrayStruct : GenericArray
   structure ArraySliceStruct : GenericArraySlice where type array = ArrayStruct.array
   structure Ring : RingBuffer where type slice = ArraySliceStruct.slice
   val conv  : T -> ArraySliceStruct.slice
   val iconv : ArraySliceStruct.slice -> T
   val zero : ArrayStruct.elem) :> TestRing
       where type T = T
         and type slice = ArraySliceStruct.slice
         and type ring = Ring.ring
=
struct
   type T = T
   type slice = ArraySliceStruct.slice
   type ring = Ring.ring
   val new = fn n => ArraySliceStruct.full (ArrayStruct.array (n,zero))
   fun write (r,l) =
      List.foldl (fn (w,a) => (Ring.write(r,conv w);(w::a))) [] l
   fun readwrite (r,l) = 
      let val buf = new size
      in List.foldl
           (fn (w,a) => (Ring.write(r,conv w);Ring.read (r,buf);(iconv buf)::a))
                  [] l
      end
end

functor SimpleTest
   (structure Ring : RingBuffer
    structure TestRing : TestRing
       where type ring = Ring.ring
         and type T = string) =
struct
   local open TestRing
      fun iterate f =
        let fun loop acc = 
          fn 0 => acc
           | n => loop (f acc) (n - 1)
        in loop
        end 
      val onetofive = ["one  ","two  ","three","four ","five "];
      val sixtoten  = ["six  ","seven","eight","nine ","ten  "];
      val r = Ring.ring 6
      val l = write (r,onetofive)
      val l = readwrite (r,sixtoten)
      val itfn = fn l => readwrite (r,l)
      val l = iterate itfn l 16
      val l1 = readwrite (r,l)
      val l2 = readwrite (r,l1)
      val l3 = readwrite (r,l2)
      val rc1 = Ring.readCount r
      val wc1 = Ring.writeCount r
      val sz1 = Ring.size r
      val av1 = Ring.avail r
      val () = Ring.resetCounters r
      val rc2 = Ring.readCount r
      val wc2 = Ring.writeCount r
      val sz2 = Ring.size r
      val av2 = Ring.avail r
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

structure Ring =
 ConcreteRingBuffer
    (structure WordStruct = WordStruct : GenericWord
     structure ArrayStruct = CharArray : GenericArray
     structure ArraySliceStruct = CharArraySlice : GenericArraySlice
     val zero = #" ");

structure Ring8 =
 ConcreteRingBuffer
    (structure WordStruct = Word8Struct : GenericWord
     structure ArrayStruct = CharArray : GenericArray
     structure ArraySliceStruct = CharArraySlice : GenericArraySlice
     val zero = #" ");

structure TR =
 TestRing
  (type T = string
   val size = 5
   structure ArrayStruct = CharArray : GenericArray
   structure ArraySliceStruct = CharArraySlice : GenericArraySlice
   structure Ring = Ring : RingBuffer
   val conv = CharArraySlice.full o CharArray.fromList o String.explode 
   val iconv = CharArraySlice.vector 
   val zero = #" ")

structure TR8 =
 TestRing
  (type T = string
   val size = 5
   structure ArrayStruct = CharArray : GenericArray
   structure ArraySliceStruct = CharArraySlice : GenericArraySlice
   structure Ring = Ring8 : RingBuffer
   val conv = CharArraySlice.full o CharArray.fromList o String.explode 
   val iconv = CharArraySlice.vector 
   val zero = #" ")

structure SimpleTest =
  SimpleTest
   (structure Ring = Ring : RingBuffer
    structure TestRing = TR : TestRing);

structure SimpleTest8 =
  SimpleTest
   (structure Ring = Ring8 : RingBuffer
    structure TestRing = TR8 : TestRing);

val res31 = SimpleTest.result;
val res8 = SimpleTest8.result;


