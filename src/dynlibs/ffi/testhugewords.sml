val _ = List.app Meta.load ["WordReprSig", "Random", "Bool",
   "MachineWord", "WordN", "AbstractMachineWord", "PrimEnc",
   "WordOps", "ArraySliceWordEnc", "VectorSliceWordEnc",
   "VectorWordEnc", "WordFormat", "IntInf", "Rot13"];

val _ = Meta.orthodox();
val _ = Meta.quietdec := true;

structure MWord=
   WordN(structure Prim = MachineWord)

structure VectorWordEnc =
   VectorWordEnc(structure PrimEnc = PrimEnc 
                 structure Word = MachineWord)
structure VectorWordPrim =
   AbstractMachineWord(structure Enc = VectorWordEnc
                       structure Ops = WordOps)
structure VectorWord=
   WordN(structure Prim = VectorWordPrim)

structure VectorWordFormat =
   WordFormat(structure Word = VectorWord);
val _ = Meta.installPP VectorWordFormat.pp;

structure Word8Format =
   WordFormat(structure Word = Word8);
val _ = Meta.installPP Word8Format.pp;

structure WordFormat =
   WordFormat(structure Word = Word);
val _ = Meta.installPP WordFormat.pp;

val _ = Meta.quietdec := false;

val toHexStr = IntInf.fmt StringCvt.HEX;
val num = IntInf.fromInt (Char.ord #"*");
val num2 = IntInf.orb(IntInf.<<(num, 32),IntInf.+(num, IntInf.fromInt 1));
val num3 = IntInf.orb(IntInf.<<(num2,64),IntInf.+(num2,IntInf.fromInt 1));
val num3'n = toHexStr num3;

val (slc,nwords,sgn) = IntInf.export IntInf.rawformat num3;

val wvarr = Array.tabulate 
              (nwords,
                 fn i => VectorWordEnc.fromWord8ArraySlice
                           (Word8ArraySlice.subslice (slc,i*4,SOME 4)));

val wvl = Array.foldl op :: [] wvarr;
val num' = IntInf.import IntInf.rawformat (slc,nwords,sgn);
val num'n = toHexStr num';

val sv = Ffi.svec_from_string  "Uryyb, Jbeyq!";
val () =  Ffi.svec_dumpb_all sv;
val sa = Ffi.svec_getbufferarray sv;
val () = Word8Array.modify Ebg13Jbeq8.qrpbqr sa;
val greetings = Ffi.svec_to_string sv;
val () =  Ffi.svec_dumpb_all sv;

val () = Ffi.ffi_dumpwords (Ffi.heap_start()) 1
val () = Ffi.ffi_dumpwords (Ffi.heap_end()) 1
val ph1 = Ffi.gc_phase();
val () = Ffi.gc_major();
val ph2 = Ffi.gc_phase();
val () = Ffi.ffi_dumpwords (Ffi.heap_start()) 1
val () = Ffi.ffi_dumpwords (Ffi.heap_end()) 1
