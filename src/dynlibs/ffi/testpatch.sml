load "ArrayPatch";
load "CharArray";
load "CharArraySlice";

structure Patch = ArrayPatch
   (structure ArrayStruct = CharArray : GenericArray
    structure ArraySliceStruct = CharArraySlice : GenericArraySlice
    val zero = #" ");

open Patch
open CharArraySlice

val arr = CharArray.tabulate (20,fn i => Char.chr(i + Char.ord(#"A")))
val slc1 = slice (arr,0,SOME 4)
val slc2 = slice (arr,4,SOME 4)
val slc3 = slice (arr,8,SOME 4)
val slc4 = slice (arr,12,SOME 4)
val slc5 = slice (arr,16,SOME 4)
val slc6 = slice (arr,12,SOME 2)
val tabfn = fn i => CharArraySlice.sub(slc4,CharArraySlice.length slc4 - i - 1)
val p3 = Patch.tabulate (CharArraySlice.length slc4,tabfn)
val p1 = fromSlice slc1
val p2 = fromSlice slc2
val p4 = fromSlice slc4
val p5 = fromSlice slc5
val p6 = fromSlice slc6
val p7 = fromSlice (slice (arr,0,SOME 8))
val p8 = fromSlice (slice (arr,16,SOME 4))
val p12 = concat [p7,p8];
val n7 = Patch.length p7;
val n8 = Patch.length p8;
val n12 = Patch.length p12;
val a12 = Patch.array p12
val sl12 = Patch.slice (p12,0,NONE);
val s12 = CharArraySlice.vector sl12;
val pp12 = Patch.patch (p12,p6,12)
val ppsl12 = Patch.slice (pp12,0,NONE);
val pps12 = CharArraySlice.vector ppsl12;
val pp121 = Patch.patch (p12,p3,0)
val ppsl121 = Patch.slice (pp121,0,NONE);
val pps121 = CharArraySlice.vector ppsl121;
val pp122 = Patch.patch (p12,p3,8)
val ppsl122 = Patch.slice (pp122,0,NONE);
val pps122 = CharArraySlice.vector ppsl122;
val pp123 = Patch.patch (p12,p3,12)
val ppsl123 = Patch.slice (pp123,0,NONE);
val pps123 = CharArraySlice.vector ppsl123;
val npp12 = Patch.length pp12
val pppatches = concat [pp121,pp122,pp123];
val _ = Patch.appi (fn (i,c) => print (String.str c)) pppatches;
val _ = print "\n";
val _ = Patch.app (fn (c) => print (String.str c)) pppatches;
val _ = print "\n";
val pppcss = Patch.foldl (fn (c,a) => (a^(String.str c))) "" pppatches;
val pppcssr = Patch.foldr (fn (c,a) => (a^(String.str c))) "" pppatches;
val pppcssr' = Patch.foldl (fn (_,(s,i)) => ((String.str (Patch.sub(pppatches,i)))^s,i+1)) ("",0) pppatches;
val pppss = Patch.slice (pppatches,0,NONE);
val pppsss = CharArraySlice.vector pppss;
val sp1 = Patch.subpatch (p12,2,SOME 4);
val slcssp1 = slices sp1;
val vsp1 = Patch.vector sp1;
val cvsp1 = CharVectorSlice.full vsp1;
val () = Patch.update(pp123,1,#"*");
val ppsl123 = Patch.slice (pp123,0,NONE);
val pps123 = CharArraySlice.vector ppsl123;

val _ = Patch.appi (fn (i,c) => print (String.str c)) pppatches;
val _ = print "\n";
val _ = Patch.app (fn (c) => print (String.str c)) pppatches;
val _ = print "\n";


val sp1 = Patch.subpatch (p1,0,SOME 1);
val slcssp1 = Patch.slices sp1;
val basessp1 = List.map CharArraySlice.base slcssp1;
val lensp1 = Patch.length sp1;
