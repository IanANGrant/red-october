load "Regex";
load "Substring";

load "StaticBuffer";

load "Scanners";
load "Printers";
load "Evil";

load "ConcRep";
load "StrmRep";
load "AbstRep";

fun dumpb wp =
   let val (arr,off,len) = StaticWord8ArraySlice.base wp
   in StaticBuffer.dumpb arr off len
   end

val dumpBuf = fn p => List.app dumpb (Patch.slices p);

functor PrinterScanner
  (type T
   structure ConcRep : ConcRep
      where type S = CharArraySlice.slice
   val valid : T -> bool
   val scan : (T -> bool) -> substring -> (T * substring) option
   val print : (ConcRep.X * string -> ConcRep.X) -> ConcRep.X * T -> ConcRep.X)
 :>
sig
   type T
   structure ConcRep : ConcRep
   val scanner : ConcRep.X -> (T * ConcRep.X) option
   val printer : ConcRep.X * T -> ConcRep.X
end where type ConcRep.S = CharArraySlice.slice
      and type T = T
      and type ConcRep.X = ConcRep.X
= struct
   local
     val consumer =
         fn (s,v) =>
              ConcRep.concat
                 [s,ConcRep.fromSlice (Evil.CharVectorSliceToCharArraySlice (Substring.full v))]
     val conv : ConcRep.X -> substring = Evil.CharArraySliceToCharVectorSlice o
            (fn p => ConcRep.slice (p,0,NONE))
     val iconv : substring -> ConcRep.X = ConcRep.fromSlice o Evil.CharVectorSliceToCharArraySlice
     val i2conv : ('a * substring) option -> ('a * ConcRep.X) option =
                fn (SOME (v,rest)) => SOME (v,iconv rest)
                 | NONE => NONE
   in
     type T = T
     structure ConcRep = ConcRep : ConcRep
      where type S = CharArraySlice.slice
     val scanner = i2conv o (scan valid) o conv
     val printer = fn (s,v) => print consumer (s,v)
   end
end

val loadInput = StringRep.fromSlice o Evil.CharVectorSliceToCharArraySlice o Substring.full

val unLoadOutput = Substring.string o
                   Evil.CharArraySliceToCharVectorSlice o
                   (fn p => StringRep.slice (p,0,NONE))

val empty = StringRep.fromList [];

val anchor = "^"
val optws = "[ \\t]*"

val match_decint = Int.scan StringCvt.DEC Substring.getc

val preproc = fn p => fn x => if p x then SOME x else NONE

val p = fn n => n >= 0 andalso n <= 255

val ipnscanner = Scanners.atomicScanner match_decint (preproc p)

val ipnprinter = Printers.atomicPrinter Int.toString

val delim = optws^"\\."

val delimre = Regex.regcomp (anchor^delim) []

val match_delim = Regex.getItem delimre []

fun preproc n = SOME n

fun postproc [] = NONE
  | postproc (l : int list) =
       if List.length l = 4
          then SOME (List.rev l)
          else NONE

val IPAddrScan = Scanners.listScanner match_delim ipnscanner preproc [] op :: postproc;

val IPAddrPrinter = Printers.listPrinter
                       (fn c => fn (acc,()) => c (acc,"."))
                       ipnprinter
                       []
                       (fn (a::rest) => (a,rest)
                         | _ => raise Fail "IPAddrPrinter: impossible")

structure IPv4AddrPrinterScanner =
   PrinterScanner
   (type T = int list
    structure ConcRep = StringRep : ConcRep
    val valid = fn _ => true (* We accept all well-formed IP addresses as valid *)
    val print = IPAddrPrinter
    val scan = (fn _ => IPAddrScan))

structure IPv4AddrStrm = 
  TextStrm(IPv4AddrPrinterScanner)

functor IPv4AddrRep
   (structure ConcRep : ConcRep
    type R
    val conv : ConcRep.Y -> R
    val iconv : R -> ConcRep.Y)
   :> AbstRep
      where type X = ConcRep.X
        and type Y = R list = (* The conversion here is at the wrong level: we should be
                                 converting whole lists, not list elements one at a time. *)
struct
      type X = ConcRep.X
      type Y = R list
      val set : Y -> X = 
          fn v => if List.length v = 4
                     then ConcRep.tabulate (4,fn i => iconv (List.nth(v,i)))
                     else raise Size
      val get : X -> Y = 
          fn arr => if ConcRep.length arr >= 4
                       then List.tabulate 
                               (4,fn i => conv(ConcRep.sub(arr,i)))
                       else raise Size
end

(* Now we make an abstract IPv4 address which represents the 4 bytes
   in a static buffer as a list of 4 ints: *)

structure IPv4AddrAbstRep =
   IPv4AddrRep(structure ConcRep = ConcRep
                type R = Int.int
                val conv = Word8.toInt
                val iconv = Word8.fromInt)

(* Let's make a static buffer - i.e. a block of memory allocated
   outside the ML heap: *)

val buf = ConcRep.array(4,0wx0f);
val () = dumpBuf buf;

(* Now we get the value of that block of memory as a list of ints via
   the abstract representation we just made *)

val ipiddr = IPv4AddrAbstRep.get buf;

(* If we change the buffer contents ... *)
val () = ConcRep.update(buf,0,0w192);
val () = ConcRep.update(buf,1,0w168);
val () = ConcRep.update(buf,2,0w0);
val () = ConcRep.update(buf,3,0w1);
val () = dumpBuf buf;

(* ... the abstract representation changes accordingly. *) 
val ipiddr = IPv4AddrAbstRep.get buf;

(* So *this* abstract representation is fairly rooted in the concrete
   representation of the static memory buffer. Nevertheless, we can
   use the stream scanner/printer to translate this abstract
   representation to an abstract representation of a string of text as
   a list of four ints: *)
 
exception Stream

structure IPv4AddrStringRep =
 TranslateAbstRep
   (structure ConcRep = StringRep : ConcRep
    structure AbstRep = IPv4AddrAbstRep : AbstRep
    structure StrmRep = IPv4AddrStrm : StrmRep
        where type S = ConcRep.X
    val conv : AbstRep.Y -> StrmRep.T = fn x => x
    val iconv : StrmRep.T -> AbstRep.Y = fn x => x
    exception Stream)

(* Now we have a new abstract representation of IPv4 addresses, also
   as lists of 4 ints, but this time one that reflects the contents of
   a text stream: *)

val inputIPAddr1 = loadInput "192.168.1.1"

(* That created a CharArray.array and loaded it with the ASCII
   representation of an IP address: *)

val cntnts = CharArraySlice.vector (StringRep.slice (inputIPAddr1,0,NONE));

(* Now we can use the new abstract representation's get function to
   get a representation of that character string as a vector of four
   integers: *)

val ipvec1 = IPv4AddrStringRep.get inputIPAddr1;

val inputIPAddr2 = loadInput "192.168.1.2"
val ipvec2 = IPv4AddrStringRep.get inputIPAddr2;

(* Of course it works the other way too, we can write out character
   string representations of IP addresses: *)

val outs = IPv4AddrStringRep.set (ipvec1);
val outs' = unLoadOutput outs;

val outs = IPv4AddrStringRep.set (ipvec2);
val outs' = unLoadOutput outs;

(* Now Words of various shapes and sizes *)

val match_decint = Word.scan StringCvt.DEC Substring.getc

val match_hexint = Word.scan StringCvt.HEX Substring.getc

val preproc = fn p => fn x => if p x then SOME x else NONE

val decWordScanner = fn p => Scanners.atomicScanner match_decint (preproc p)
val hexWordScanner = fn p => Scanners.atomicScanner match_hexint (preproc p)

val hexPref = optws^"0x"

val hexPrefRe = Regex.regcomp (anchor^hexPref) []

val match_hexPref = Regex.getItem hexPrefRe []

val preproc = (fn l => SOME l)
          
fun postproc (elt2) = (SOME elt2)

val hexWordScan = fn p =>
     Scanners.seqScanner
        match_hexPref
        (hexWordScanner p)
        preproc
        preproc
        (fn (_,x) => x)
        postproc

val decWordScan = decWordScanner

val WordScanner = fn p => Scanners.altScanner (hexWordScan p) (decWordScan p) preproc preproc postproc

val hexWordPrinter = fn c => fn (acc,n) => c (acc,Word.fmt StringCvt.HEX n)

val WordPrinter =
     Printers.seqPrinter 
        (fn c => fn (acc,()) => c (acc, "0x"))
        hexWordPrinter
        (fn p => ((),p))

val p4 = fn n => n >= 0w0 andalso n <= 0wxf
val p8 = fn n => n >= 0w0 andalso n <= 0wxff
val p16 = fn n => n >= 0w0 andalso n <= 0wxffff
val p32 = fn n => n >= 0w0 andalso n <= 0wx3fffffff

structure Word4PrinterScanner =
   PrinterScanner
    (type T = Word.word
     structure ConcRep = StringRep : ConcRep
     val valid = p4
     val print = WordPrinter
     val scan = WordScanner)

structure Word8PrinterScanner =
   PrinterScanner
    (type T = Word.word
     structure ConcRep = StringRep : ConcRep
     val valid = p8
     val print = WordPrinter
     val scan = WordScanner)

structure Word16PrinterScanner =
   PrinterScanner
    (type T = Word.word
     structure ConcRep = StringRep : ConcRep
     val valid = p16
     val print = WordPrinter
     val scan = WordScanner)

structure Word32PrinterScanner =
   PrinterScanner
    (type T = Word.word
     structure ConcRep = StringRep : ConcRep
     val valid = p32
     val print = WordPrinter
     val scan = WordScanner)

structure Word4Strm = 
  TextStrm(Word4PrinterScanner)

structure Word8Strm = 
  TextStrm(Word8PrinterScanner)

structure Word16Strm = 
  TextStrm(Word16PrinterScanner)

structure Word32Strm = 
  TextStrm(Word32PrinterScanner)

val input = loadInput "0x2a 0x2a";
val SOME (res1,rest) = Word8Strm.getItem input;
val SOME (res2,rest) = Word8Strm.getItem rest;

val outs = Word8Strm.setItem (empty,res1);
val outs' = unLoadOutput outs;
val outs = Word8Strm.setItem (empty,res2);
val outs' = outs'^" "^(unLoadOutput outs);

val input16 = loadInput "0x2a2a 0x2a2b";
val SOME (res161,rest) = Word16Strm.getItem input16;
val SOME (res162,rest) = Word16Strm.getItem rest;

val outs = Word16Strm.setItem (empty,res161);
val outs' = unLoadOutput outs;
val outs = Word16Strm.setItem (empty,res162);
val outs' = outs'^" "^(unLoadOutput outs);

val inputIPAddr = loadInput "192.168.1.1 192.168.1.2"
val SOME (resIPA1,rest) = IPv4AddrStrm.getItem inputIPAddr
val SOME (resIPA2,rest) = IPv4AddrStrm.getItem rest;

val outs = IPv4AddrStrm.setItem (empty,resIPA1);
val outs' = unLoadOutput outs;
val outs = IPv4AddrStrm.setItem (empty,resIPA2);
val outs' = outs'^" "^(unLoadOutput outs);
