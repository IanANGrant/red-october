val _ = List.app Meta.load 
["WordReprSig", "Random", "Bool", "WordN", "AbstractMachineWord",
 "PrimEnc", "WordOps", "ArraySliceWordEnc", "VectorSliceWordEnc",
 "VectorWordEnc", "WordFormat", "IntInf"];

val _ = Meta.orthodox();
val _ = Meta.quietdec := true;

structure VectorWordEnc = VectorWordEnc(structure PrimEnc = PrimEnc)
structure VectorWordPrim = AbstractMachineWord(structure Enc = VectorWordEnc
                                               structure Ops = WordOps)
  :> WordPrim where type word = VectorWordEnc.word
structure VectorWord=WordN(structure Prim = VectorWordPrim)

structure ArraySliceWordEnc = ArraySliceWordEnc(structure PrimEnc = PrimEnc)
structure ArraySliceWordPrim = AbstractMachineWord(structure Enc = ArraySliceWordEnc
                                                   structure Ops = WordOps)
  :> WordPrim where type word = ArraySliceWordEnc.word
structure ArraySliceWord=WordN(structure Prim = ArraySliceWordPrim)

structure Word8Format = WordFormat(structure Word = Word8);
val _ = Meta.installPP Word8Format.pp;

structure WordFormat = WordFormat(structure Word = Word);
val _ = Meta.installPP WordFormat.pp;

structure VectorWordFormat = WordFormat(structure Word = VectorWord);
val _ = Meta.installPP VectorWordFormat.pp;

structure ArraySliceWordFormat = WordFormat(structure Word = ArraySliceWord);
val _ = Meta.installPP ArraySliceWordFormat.pp;

val _ = Meta.quietdec := false;

val toHexStr = IntInf.fmt StringCvt.HEX
val num = IntInf.fromInt (Char.ord #"*")
val num2 = IntInf.orb(IntInf.<<(num, 32),IntInf.+(num, IntInf.fromInt 1))
val num3 = IntInf.orb(IntInf.<<(num2,64),IntInf.+(num2,IntInf.fromInt 1))
val num3'n = toHexStr num3
val (slc,nwords,sgn) = IntInf.export IntInf.rawformat num3
val wvarr = Array.tabulate 
              (nwords,
                 fn (i) => VectorWordEnc.fromWord8ArraySlice
                            (Word8ArraySlice.subslice (slc,i*4,SOME 4)))
val num' = IntInf.import IntInf.rawformat (slc,nwords,sgn)
val num'n = toHexStr num'
