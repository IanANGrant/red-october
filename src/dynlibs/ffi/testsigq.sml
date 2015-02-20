val _ = List.app load
   ["MemDebug",    "Vector",               "PolyRedBlackMap",
    "Array",       "ArraySlice",           "RealRepr",
    "SigQFifo",    "GenericArray",         "GenericArraySlice",
    "ObjRepr",     "MappedNativeRegister", "DoubleMappedWord8Array"];

structure DMappedWord8Array :> GenericArray
   where type array = MappedWord8Array.array
     and type elem = Word8.word
     and type vector = Word8Vector.vector =
struct
   open MappedWord8Array
   val array = DoubleMappedWord8Array.array
end

structure WordStruct
   :> GenericWord
        where type largeword = Word.word
          and type word = Word.word =
struct
   type largeword = Word.word
   open Word
end

structure Fifo =
 SigQFifo
   (structure WordStruct = WordStruct
    structure ArrayStruct = DMappedWord8Array
    structure ArraySliceStruct = MappedWord8ArraySlice
    val block_on_read : int -> unit = fn i => print ("blocked on read "^(Int.toString i)^"\n")
    val block_on_write : int -> unit = fn i => print ("blocked on write "^(Int.toString i)^"\n")
    val signal_read : int -> unit  = fn i => print ("signal_read "^(Int.toString i)^"\n")
    val signal_write : int -> unit = fn i => print ("signal write "^(Int.toString i)^"\n")
    val length = Word8Vector.length
    val appi = Word8Vector.appi
    val get_cptr : ArrayStruct.array -> Dynlib.cptr = MappedWord8Array.get_cptr
    val zero : ArrayStruct.elem = 0w0)

val dumpFifo = Fifo.dump

structure ObjRepr =
   ObjRepr
     (type state = Fifo.fifo
      val readByte = Fifo.readByte
      val writeByte = Fifo.writeByte)

fun testCodec (buff : Fifo.fifo) (obj : 'a) : 'a =
   let val _ = ObjRepr.encode (obj,buff)
       val _ = MemDebug.gc_full_major()
   in ObjRepr.decode buff 
   end

fun mkRegisterVector n slc =
   let open MappedNativeRegister
       open MappedWord8ArraySlice
       val rl = (Word.wordSize + 7) div 8
   in if length slc < rl 
         then raise Size
         else Vector.tabulate (n,fn i => new (subslice(slc,i*4,SOME rl)))
   end

val nregs = 2
val wbytes = (Word.wordSize + 7) div 8

val arr = MappedWord8Array.array(nregs*wbytes,0w0);

val regsslc = MappedWord8ArraySlice.full arr;
val regsv = mkRegisterVector 2 (regsslc);
val regs = (Vector.sub (regsv,0),Vector.sub (regsv,1));

val buff = Fifo.fifo (12,regs);
val slc = Fifo.buffer buff;
val l = MappedWord8ArraySlice.length(slc);
val _ = MappedWord8ArraySlice.update (slc,4096,0wx2a);
val 0wx2a = MappedWord8ArraySlice.sub (slc,0);

 val s : string = testCodec buff "abcdefg"; 

val ("abcdefg",42.0,0wx7fffffff) = testCodec buff ("abcdefg",42.0,0wx7fffffff);

