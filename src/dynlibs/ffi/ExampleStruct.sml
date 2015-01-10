load "Regex";
load "StringCvt";

load "GenericArraySlice_sig";
load "GenericArray_sig";

load "StaticBuffer";
load "StaticWord8ArraySlice";
load "CharArraySlice";
load "ArrayPatch";
load "BitSet";

load "ConcRep";
load "StrmRep";
load "AbstRep";

load "TCPConsts";
load "IPv4Consts";

load "IPv4Packet";

StaticBuffer.set_debug false;

functor VectorAbstRep
   (structure ConcRep : ConcRep
    type R
    type Y
    val conv : ConcRep.Y -> R
    val iconv : R -> ConcRep.Y
    val length : Y -> int 
    val sub : (Y * int) -> R
    val tabulate : int * (int -> R) -> Y
    val bigendian : bool)
 :> AbstRep
       where type X = ConcRep.X
         and type Y = Y
= struct
   type X = ConcRep.X
   type Y = Y
   local fun tabulatefn idxfn l vec =
                ConcRep.tabulate(l,fn i => iconv (sub (vec,idxfn i)))
         fun subfn idxfn arr =
                fn i => conv (ConcRep.sub(arr,idxfn i))
         fun indexfn l =
                 if bigendian
                    then fn i => i
                    else fn i => l - 1 - i
   in fun set vec = 
         let val l = length vec
         in  tabulatefn (indexfn l) l vec
         end
      fun get arr =
         let val l = ConcRep.length arr
         in tabulate (l, subfn (indexfn l) arr)
         end
     end
end

functor WordRep
   (structure ConcRep : ConcRep
    val conv : ConcRep.Y -> Word.word
    val iconv : Word.word -> ConcRep.Y
    val length : Int.int
    val bigendian : bool)
   :> AbstRep
      where type X = ConcRep.X
        and type Y = Word.word =
struct
   local fun bits w =
             let fun iter n 0w0 = n
                   | iter n w = iter (n+1) (Word.>>(w,0w1))
             in iter 0 w
             end
   in
      type X = ConcRep.X
      type Y = Word.word
      val set : Y -> X = 
          fn w => 
            let val l = (length + 7) div 8
                val indexfn = if bigendian
                                 then (fn i => i)
                                 else (fn i => l - i - 1)
                fun tabulatefn w =
                    fn i => let val b = indexfn i
                            in iconv (Word.andb((Word.>>(w,Word.*(Word.fromInt b,0w8))),0wxff))
                            end
            in if bits w > length 
                  then raise Size
                  else ConcRep.tabulate (l,tabulatefn w)
            end
          val get : X -> Y = fn arr =>
            let val nbits = 8 * (ConcRep.length arr)
                val fold = if bigendian then ConcRep.foldr else ConcRep.foldl
                val foldfn = (fn (w,a) => Word.orb(Word.<<(a,0w8), conv w))
            in if nbits < length
                  then raise Size
                  else fold foldfn 0w0 arr
            end
        end
end

functor IPv4AddrRep
   (structure ConcRep : ConcRep
    type R
    val conv : ConcRep.Y -> R
    val iconv : R -> ConcRep.Y)
   :> AbstRep
      where type X = ConcRep.X
        and type Y = R Vector.vector =
struct
      type X = ConcRep.X
      type Y = R Vector.vector
      val set : Y -> X = 
          fn v => if Vector.length v = 4
                     then ConcRep.tabulate (4,fn i => iconv (Vector.sub(v,i)))
                     else raise Size
      val get : X -> Y = 
          fn arr => if ConcRep.length arr >= 4
                       then Vector.tabulate 
                               (4,fn i => conv(ConcRep.sub(arr,i)))
                       else raise Size
end

functor IPv4StringAddrAbstRep
   (structure ConcRep : ConcRep
    structure IPv4AddrAbstRep : AbstRep 
       where type X = ConcRep.X
         and type Y = Word.word Vector.vector)
   :> AbstRep
      where type X = ConcRep.X
        and type Y = string =
struct
      type X = ConcRep.X
      type Y = string
      local
         val optws = "[ \\t]*"
         val numbr = "\\([0-9]\\{1,3\\}\\)"
         val dot = "\\."
         val anchor = "^"
         val sep = optws^dot^optws
         val nmbrs = numbr^sep^numbr^sep^numbr^sep^numbr
         val ipaddr = anchor^optws^nmbrs
         val ipaddrre = Regex.regcomp ipaddr []
         val scanip = Regex.regexec ipaddrre []
         fun sconv s =
             let val f = Option.valOf o (StringCvt.scanString (Word.scan StringCvt.DEC))
                 val w = f s
             in if w <= 0wxFF
                then w
                else raise Fail ("Invalid IPv4 address component "^(Word.toString w))
             end
         fun getscannedip s = 
               case scanip s
                 of SOME v => Vector.map Substring.string v
                  | NONE => raise Fail ("Invalid IPv4 address "^s)
         fun parseip s =
             let val scannedip = getscannedip s
                 val ip = Vector.tabulate (4,fn i => sconv (Vector.sub (scannedip,i+1)))
             in ip 
             end
         fun printip v =
             let fun appfn (w,a) =
                 let val sep = if a = "" then "" else "."
                 in a^sep^(Word.fmt StringCvt.DEC w)
                 end
             in if Vector.length v = 4
                   then Vector.foldl appfn "" v
                   else "*** IPv4 Address Invalid ***"
             end
      in
         val set : Y -> X = IPv4AddrAbstRep.set o parseip
         val get : X -> Y = printip o IPv4AddrAbstRep.get
   end
end

functor WordBitsAbstRep
  (structure ConcRep : ConcRep
   structure BitEnum : BitEnum
   structure WordAbstRep : AbstRep where type X = ConcRep.X
                                         and type Y = Word.word)
      :> AbstRep where type X = ConcRep.X
                    and type Y = BitEnum.Enum.enum list =
struct
   type X = ConcRep.X
   type Y = BitEnum.Enum.enum list
   val get = BitEnum.fromWord o WordAbstRep.get
   val set = WordAbstRep.set o BitEnum.toWord
end

functor StringWordBitsAbstRep
  (structure ConcRep : ConcRep
   structure BitEnum : BitEnum
   structure WordBitsAbstRep : AbstRep where type X = ConcRep.X
                                         and type Y = BitEnum.Enum.enum list)
      :> AbstRep where type X = ConcRep.X
                    and type Y = string =
struct
   type X = ConcRep.X
   type Y = string
   val get = BitEnum.toString o WordBitsAbstRep.get
   val set = WordBitsAbstRep.set o BitEnum.fromString
end

functor WordPairBitUnionAbstRep
  (structure ConcRep : ConcRep
   structure WordBits1 : AbstRep where type X = ConcRep.X
   structure WordBits2 : AbstRep where type X = ConcRep.X
   structure WordRep : AbstRep where type X = ConcRep.X
                                 and type Y = Word.word)
      :> AbstRep where type X = ConcRep.X
                   and type Y = WordBits1.Y * WordBits2.Y =
struct
   type X = ConcRep.X
   type Y = WordBits1.Y * WordBits2.Y
   fun get (x : X) = 
         let val xs = WordRep.get x
         in (WordBits1.get x,WordBits2.get x)
         end
   fun set ((y1,y2) : Y) =
          WordRep.set 
            (Word.orb(WordRep.get (WordBits1.set y1),
                      WordRep.get (WordBits2.set y2)))
end

signature BitField =
sig
   type X
   type Y
   val getWord : X -> Y
   val setWord : X * Y -> X
end

functor WordBitField
   (type Y
    val conv : Y -> Word.word
    val iconv : Word.word -> Y 
    val offset : int
    val length : int
    val bigendian : bool) 
      :> BitField where type X = Word.word
                    and type Y = Y =
struct
   type X = Word.word
   type Y = Y
   local 
      val fst = if bigendian
                   then Word.wordSize - (offset+length-1)
                   else offset
      fun ones n = Word.-(Word.<<(0w1,Word.fromInt n),0w1)
      val shift = Word.fromInt(fst)
      val mask = Word.<<(ones length,shift)
      val nmask = Word.notb mask
      fun getbits w = Word.>>(Word.andb(w,mask),shift)
      fun setbits (w,b) = Word.orb(Word.andb(nmask,w),Word.<<(b,shift))
   in
      fun getWord w = iconv (getbits w)
      fun setWord (w,t) = (setbits (w,conv t))
   end
end

functor WordBitFieldAbstRep
  (structure ConcRep : ConcRep
   structure BitField : BitField where type X = Word.word
   structure WordAbstRep : AbstRep where type X = ConcRep.X
                                     and type Y = Word.word)
      :> AbstRep where type X = ConcRep.X
                    and type Y = BitField.Y =
struct
   type X = ConcRep.X
   type Y = BitField.Y
   val get = BitField.getWord o WordAbstRep.get
   val set = fn bs => WordAbstRep.set (BitField.setWord (0w0,bs))
end

functor WordBitFieldPairAbstRep
  (structure ConcRep : ConcRep
   structure BitField1 : BitField where type X = Word.word
   structure BitField2 : BitField where type X = Word.word
   structure WordAbstRep : AbstRep where type X = ConcRep.X
                                      and type Y = Word.word)
      :> AbstRep where type X = ConcRep.X
                    and type Y = BitField1.Y * BitField2.Y =
struct
   type X = ConcRep.X
   type Y = BitField1.Y * BitField2.Y
   fun get (x : X) = 
         let val xs = WordAbstRep.get x
         in (BitField1.getWord xs,BitField2.getWord xs)
         end
   fun set ((y1,y2) : Y) =
         let val y = BitField1.setWord (0w0,y1)
             val ys = BitField2.setWord (y,y2)
         in WordAbstRep.set ys
         end
end

structure IHFlagBits =
   BitSet(structure Enum = IPv4Consts)

open IPv4Consts

functor IPv4PacketAbstRep
   (structure ConcRep : ConcRep
       where type Y = Word8.word) =
struct
    structure Word32AbstRep =
   WordRep(structure ConcRep = ConcRep
            val conv = Word8.toLargeWord
            val iconv = Word8.fromLargeWord
            val length = 30
           val bigendian = false)
    structure Word32Strm =
   AbstConcConcStrmRep
                (structure ConcRep = ConcRep : ConcRep
                 structure AbstRep = Word32AbstRep : AbstRep
                 val length = 4)
      structure Word16AbstRep =
        WordRep(structure ConcRep = ConcRep
                val conv = Word8.toLargeWord
                val iconv = Word8.fromLargeWord
                val length = 16
                val bigendian = false)
    structure Word16Strm =
   AbstConcConcStrmRep
                (structure ConcRep = ConcRep : ConcRep
                 structure AbstRep = Word16AbstRep : AbstRep
                 val length = 2)
      structure Word8AbstRep =
        WordRep(structure ConcRep = ConcRep
                val conv = Word8.toLargeWord
                val iconv = Word8.fromLargeWord
                val length = 8
                val bigendian = false)
    structure Word8Strm =
   AbstConcConcStrmRep
                (structure ConcRep = ConcRep : ConcRep
                 structure AbstRep = Word8AbstRep : AbstRep
                 val length = 1)
     structure IPv4AddrAbstRep =
   IPv4AddrRep(structure ConcRep = ConcRep
                type R = Word.word
                val conv = Word8.toLargeWord
                val iconv = Word8.fromLargeWord)
     structure IPv4StringAddrAbstRep =
   IPv4StringAddrAbstRep
                (structure ConcRep = ConcRep : ConcRep
                 structure IPv4AddrAbstRep =
                             IPv4AddrAbstRep : AbstRep)
     structure IPv4AddrStrm =
   AbstConcConcStrmRep
                (structure ConcRep = ConcRep : ConcRep
                 structure AbstRep = IPv4StringAddrAbstRep : AbstRep
                 val length = 4)
      structure HdrLenBits =
        WordBitField
         (type Y = Word.word
          val conv : Y -> Word.word = fn x => x
          val iconv : Word.word -> Y = fn x => x
          val offset = 0
          val length = 4
          val bigendian = false)
      structure HdrLenField = 
       WordBitFieldAbstRep
        (structure ConcRep = ConcRep : ConcRep
         structure BitField = HdrLenBits : BitField
         structure WordAbstRep = Word8AbstRep : AbstRep)
            :> AbstRep where type X = ConcRep.X
                         and type Y = HdrLenBits.Y
      structure VersionBits =
        WordBitField
         (type Y = Word.word
          val conv : Y -> Word.word = fn x => x
          val iconv : Word.word -> Y = fn x => x
          val offset = 4
          val length = 4
          val bigendian = false)
      structure VersionField = 
       WordBitFieldAbstRep
        (structure ConcRep = ConcRep : ConcRep
         structure BitField = VersionBits : BitField
         structure WordAbstRep = Word8AbstRep : AbstRep)
            :> AbstRep where type X = ConcRep.X
                         and type Y = VersionBits.Y
      structure OffsBits =
        WordBitField
         (type Y = Word.word
          val conv : Y -> Word.word = fn x => x
          val iconv : Word.word -> Y = fn x => x
          val offset = 0
          val length = 13
          val bigendian = false)
      structure OffsField = 
       WordBitFieldAbstRep
        (structure ConcRep = ConcRep : ConcRep
         structure BitField = OffsBits : BitField
         structure WordAbstRep = Word16AbstRep : AbstRep)
            :> AbstRep where type X = ConcRep.X
                         and type Y = OffsBits.Y
      structure FlagBits =
        WordBitField
         (type Y = IPv4Consts.enum option
          val conv : Y -> Word.word = 
                           fn SOME c => IPv4Consts.toWord c
                            | NONE => 0w0
          val iconv : Word.word -> Y =
                           fn w => SOME (IPv4Consts.fromWord w)
                                    handle Fail _ => NONE
          val offset = 0
          val length = 16
          val bigendian = false)
      structure FlagsField = 
       WordBitFieldAbstRep
        (structure ConcRep = ConcRep : ConcRep
         structure BitField = FlagBits : BitField
         structure WordAbstRep = Word16AbstRep : AbstRep)
            :> AbstRep where type X = ConcRep.X
                         and type Y = IPv4Consts.enum option

structure FlagsFieldWord =
WordBitsAbstRep
  (structure ConcRep = ConcRep : ConcRep
   structure BitEnum = IHFlagBits : BitEnum
   structure  WordAbstRep = Word16AbstRep : AbstRep
      where type X = ConcRep.X
        and type Y = Word.word)

structure FlagsFieldString =
 StringWordBitsAbstRep
  (structure ConcRep = ConcRep : ConcRep
   structure BitEnum = IHFlagBits : BitEnum
   structure WordBitsAbstRep = FlagsFieldWord : AbstRep
      where type X = ConcRep.X
        and type Y = BitEnum.Enum.enum list)
      :> AbstRep where type X = ConcRep.X
                    and type Y = string

structure FragAbstRep2 =
WordPairBitUnionAbstRep
  (structure ConcRep = ConcRep : ConcRep
   structure WordBits1 = FlagsFieldString : AbstRep where type X = ConcRep.X
   structure WordBits2 = OffsField : AbstRep where type X = ConcRep.X
   structure WordRep = Word16AbstRep : AbstRep where type X = ConcRep.X
                                       and type Y = Word.word)

structure HdrAbstRep =
  WordBitFieldPairAbstRep
  (structure ConcRep = ConcRep : ConcRep
   structure BitField1 = HdrLenBits  : BitField where type X = Word.word
   structure BitField2 = VersionBits : BitField where type X = Word.word
   structure WordAbstRep = Word8AbstRep : AbstRep where type X = ConcRep.X
                                     and type Y = Word.word)
      :> AbstRep where type X = ConcRep.X
                   and type Y = HdrLenBits.Y * VersionBits.Y
structure HdrStrm =
   AbstConcConcStrmRep
                (structure ConcRep = ConcRep : ConcRep
                 structure AbstRep = HdrAbstRep : AbstRep
                 val length = 1)
structure FragAbstRep =
  WordBitFieldPairAbstRep
  (structure ConcRep = ConcRep : ConcRep
   structure BitField1 = OffsBits : BitField where type X = Word.word
   structure BitField2 = FlagBits : BitField where type X = Word.word
   structure WordAbstRep = Word16AbstRep : AbstRep where type X = ConcRep.X
                                     and type Y = Word.word)
      :> AbstRep where type X = ConcRep.X
                   and type Y = OffsBits.Y * FlagBits.Y
structure FragStrm2 =
   AbstConcConcStrmRep
                (structure ConcRep = ConcRep : ConcRep
                 structure AbstRep = FragAbstRep2 : AbstRep
                 val length = 2)

structure PayloadAbstRep =
    VectorAbstRep
            (structure ConcRep = ConcRep
             type R = string
             type Y = string Vector.vector
             val conv = (StringCvt.padLeft #"0" 2) o Word8.toString
             val iconv = Option.valOf o Word8.fromString
             val appi : (int * string -> unit) -> string Vector.vector -> unit = Vector.appi
             val length : string Vector.vector -> int = Vector.length
             val sub : (string Vector.vector * int) -> string = Vector.sub
             val tabulate : int * (int -> string) -> string Vector.vector = Vector.tabulate
             val bigendian = false)      

structure PayloadStrm =
   AbstConcConcStrmRep
                (structure ConcRep = ConcRep : ConcRep
                 structure AbstRep = PayloadAbstRep : AbstRep
                 val length = 2)

structure Conc =
  ConcIPv4Packet
   (structure ConcRep = ConcRep : ConcRep)
      :> IPv4Packet
        where type hdr = ConcRep.X
          and type tos = ConcRep.X
          and type length = ConcRep.X
          and type id = ConcRep.X
          and type frag = ConcRep.X
          and type ttl = ConcRep.X
          and type protocol = ConcRep.X
          and type checksum = ConcRep.X
          and type address = ConcRep.X
          and type options = ConcRep.X
          and type payload = ConcRep.X

structure Abst =
 AbstIPv4Packet
   (structure ConcRep = ConcRep : ConcRep
    structure Packet = Conc : IPv4Packet 
    type hdr = Word.word * Word.word
    type tos = Word.word
    type length = Word.word
    type id = Word.word
    type frag = String.string * Word.word
    type ttl = Word.word
    type protocol = Word.word
    type checksum = Word.word
    type address = string
    type options = Word.word
    type payload = string Vector.vector
    structure Hdr = HdrAbstRep : AbstRep
      where type X = ConcRep.X
        and type Y = hdr
    structure Tos = Word8AbstRep : AbstRep 
     where type X = ConcRep.X
       and type Y = tos
    structure Length = Word16AbstRep : AbstRep 
     where type X = ConcRep.X
       and type Y = length
    structure Id = Word16AbstRep : AbstRep 
     where type X = ConcRep.X
       and type Y = id
    structure Frag = FragAbstRep2 : AbstRep 
     where type X = ConcRep.X
       and type Y = frag
    structure Ttl = Word8AbstRep : AbstRep 
     where type X = ConcRep.X
       and type Y = ttl
    structure Protocol = Word8AbstRep : AbstRep 
     where type X = ConcRep.X
       and type Y = protocol
    structure Checksum = Word16AbstRep : AbstRep 
     where type X = ConcRep.X
       and type Y = checksum
    structure Address = IPv4StringAddrAbstRep : AbstRep 
     where type X = ConcRep.X
       and type Y = address
    structure Options = Word32AbstRep : AbstRep 
     where type X = ConcRep.X
       and type Y = options
    structure Payload = PayloadAbstRep : AbstRep
     where type X = ConcRep.X
       and type Y = payload) : IPv4Packet

structure Strm =
  IPv4PacketStrm
   (structure ConcRep = ConcRep : ConcRep
    type hdr = Word.word * Word.word
    type tos = Word.word
    type length = Word.word
    type id = Word.word
    type frag = String.string * Word.word
    type ttl = Word.word
    type protocol = Word.word
    type checksum = Word.word
    type address = string
    type options = Word.word
    type payload = string Vector.vector
    structure Hdr = HdrStrm.Abst : StrmRep
      where type S = ConcRep.X
        and type T = hdr
    structure Tos = Word8Strm.Abst : StrmRep 
     where type S = ConcRep.X
       and type T = tos
    structure Length = Word16Strm.Abst : StrmRep 
     where type S = ConcRep.X
       and type T = length
    structure Id = Word16Strm.Abst : StrmRep 
     where type S = ConcRep.X
       and type T = id
    structure Frag = FragStrm2.Abst : StrmRep 
     where type S = ConcRep.X
       and type T = frag
    structure Ttl = Word8Strm.Abst : StrmRep 
     where type S = ConcRep.X
       and type T = ttl
    structure Protocol = Word8Strm.Abst : StrmRep 
     where type S = ConcRep.X
       and type T = protocol
    structure Checksum= Word16Strm.Abst : StrmRep 
     where type S = ConcRep.X
       and type T = checksum
    structure Address = IPv4AddrStrm.Abst : StrmRep 
     where type S = ConcRep.X
       and type T = address
    structure Options = Word32Strm.Abst : StrmRep 
     where type S = ConcRep.X
       and type T = options
    structure Payload = PayloadStrm.Abst : StrmRep 
     where type S = ConcRep.X
       and type T = payload)
end

structure Packet =
  IPv4PacketAbstRep
    (structure ConcRep = ConcRep : ConcRep)

fun dumpb wp =
   let val (arr,off,len) = StaticWord8ArraySlice.base wp
   in StaticBuffer.dumpb arr off len
   end

val buf = ConcRep.array(32,0w0);
val SOME (pkt,free) = Packet.Strm.getItem buf;

val nap = Packet.Abst.new pkt;
val () = Packet.Abst.set_srcaddress (nap,"192.168.0.1");
val () = Packet.Abst.set_dstaddress (nap,"192.168.0.2");
val () = Packet.Abst.set_frag (nap,("DF",0wxef));
val () = Packet.Abst.set_hdr (nap,(0wx4,0wxf));
val napr = Packet.Abst.get nap;
val () = Packet.Abst.set (nap,napr);
val _ = List.map dumpb (Patch.slices (slc));
val _ = List.map dumpb (Patch.slices (buf));

val p = Packet.Abst.new
             ((0w1,0w4),0w1,0w1,0w1,("DF|MF",0wxFF),0w1,0w1,0w1,
              "127.0.0.1","127.0.0.1",0w2,#["00", "00", "00"]);

val ap = Packet.Abst.get p;
val cp = Packet.Conc.get p;

val buf2 = ConcRep.fromList [];
val (slc2) = Packet.Strm.setItem (buf2,ap);
val lp2 = ConcRep.length slc2;
val bases = List.map StaticWord8ArraySlice.base (Patch.slices slc2);

val _ = List.map dumpb (Patch.slices (slc2));
