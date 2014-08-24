val _ = List.app Meta.load ["TestWord", "WordN", "Jit"];

val _ = Meta.orthodox();
val _ = Meta.quietdec := true;

val _ = Jit.jit_set_memory_functions Ffi.my_alloc Ffi.my_realloc Ffi.my_free

val () = Jit.init_jit Jit.argv0;

val _ = print "\nInstantiating structures: "
val _ = print "\n  RefWord"
structure RefWord = 
struct
   type largeword = Word.word
   open Word
end

val _ = print "\n  MachineWord"
structure MWord=
   WordN(structure Prim = MachineWord)

val _ = print "\n  VectorWord"
structure VectorWordEnc =
   VectorWordEnc(structure PrimEnc = PrimEnc 
                 structure Word = MachineWord)
structure VectorWordPrim =
   AbstractMachineWord(structure Enc = VectorWordEnc
                       structure Ops = WordOps)
structure VectorWord=
   WordN(structure Prim = VectorWordPrim)

val _ = print "\n  ArraySliceWord"
structure ArraySliceWordEnc = 
   ArraySliceWordEnc(structure PrimEnc = PrimEnc
                     structure Word = MachineWord)
structure ArraySliceWordPrim =
   AbstractMachineWord(structure Enc = ArraySliceWordEnc
                       structure Ops = WordOps)
structure ArraySliceWord=
   WordN(structure Prim = ArraySliceWordPrim)

val _ = print "\n  VectorSliceWord"
structure VectorSliceWordEnc =
   VectorSliceWordEnc(structure PrimEnc = PrimEnc 
                      structure Word = MachineWord)
structure VectorSliceWordPrim =
   AbstractMachineWord(structure Enc = VectorSliceWordEnc 
                       structure Ops = WordOps)
structure VectorSliceWord=
   WordN(structure Prim = VectorSliceWordPrim)

val _ = print "\n";

val _ = print "\nInstantiating tests: "
val _ = print "\n  MachineWord"
structure TestWord=
   TestWord(structure WordA = MWord
            structure WordB = RefWord)

val _ = print "\n  VectorWord"
structure TestVectorWord=
   TestWord(structure WordA = VectorWord
            structure WordB = MWord)

val _ = print "\n  ArraySliceWord"
structure TestArraySliceWord=
   TestWord(structure WordA = ArraySliceWord
            structure WordB = VectorWord)

val _ = print "\n  VectorSliceWord"
structure TestVectorSliceWord=
   TestWord(structure WordA = VectorSliceWord
            structure WordB = ArraySliceWord)

val _ = print "\n  RefWord"
structure TestRefWord=
   TestWord(structure WordA = RefWord
            structure WordB = VectorSliceWord)

val _ = print "\n";

local
   local
      open IntInf
   in (* Poor man's rational arithmetic. If it's this easy,
         then what's the floating point? *)
      val zero = fromInt 0
      val one = fromInt 1
      val hundred = fromInt 100
      fun ratnorm (n,d) =
         let val g = gcd (n,d)
         in (n div g, d div g)
         end
      fun listavg l =
        let val iter = fn (n,(a,m)) => ratnorm (a + (fromInt n), m + one)
        in List.foldl iter (zero,zero) l
        end
      fun ratimul (i,(n,d)) = ratnorm (i * n,d)
      fun ratdiv ((n,d),(n',d')) = ratnorm (n * d', n' * d)
      fun ratToInt (n,d) = toInt (n div d)
      fun ratToIntString (n,d) = toString(n div d)
      fun ratToString (n,d) = (toString n)^"/"^(toString d)
   end
   val tests = 
        [(TestWord.runTests,            "MachineWord",     "RefWord"),
         (TestVectorWord.runTests,      "VectorWord",      "MachineWord"),
         (TestArraySliceWord.runTests,  "ArraySliceWord",  "VectorWord"),
         (TestVectorSliceWord.runTests, "VectorSliceWord", "ArraySliceWord"),
         (TestRefWord.runTests,         "RefWord",         "VectorSliceWord")]
   fun padname s = UTF8.padRight #" " 15 s
   fun runtest (thunk,s1,s2) =
         (print ("  "^(padname s1)^" against "^(padname s2)^" ");
          thunk();
          print " OK.\n")
   fun runtests l = (print "\nTesting:\n";List.app runtest tests)
   fun cdn n =
      let val nr = ref n
          fun ncdn () = (print (" "^(Int.toString (!nr)));nr := ((!nr)-1))
      in ncdn end
   val cdt = cdn 10
   val _ = runtests tests
   val _ = print "\nTiming:"
   val mcwtimes =  [TestWord.timeA(cdt()),            TestVectorWord.timeB(cdt())]
   val vecwtimes = [TestVectorWord.timeA(cdt()),      TestArraySliceWord.timeB(cdt())]
   val vswtimes =  [TestVectorSliceWord.timeA(cdt()), TestRefWord.timeB(cdt())]
   val aswtimes =  [TestArraySliceWord.timeA(cdt()),  TestVectorSliceWord.timeB(cdt())]
   val refwtimes = [TestRefWord.timeA(cdt()),         TestWord.timeB(cdt())]
   val _ = print ".\n\n"
   val avgrefwu = listavg refwtimes
   val avgmcwu = listavg mcwtimes
   val avgvecwu = listavg vecwtimes
   val avgaswu = listavg aswtimes
   val avgvswu = listavg vswtimes
   val ref100 = ratimul (hundred, avgrefwu)
   val refwdpc = ratdiv (ref100, avgrefwu) (* Sanity check (but whose sanity?). *)
   val mcwdpc = ratdiv (ref100, avgmcwu)
   val vwdpc = ratdiv (ref100, avgvecwu)
   val vswdpc = ratdiv (ref100, avgvswu)
   val aswdpc = ratdiv (ref100, avgaswu)
   val timeres = [("RefWord",        refwdpc, avgrefwu),
                  ("MachineWord",    mcwdpc,  avgmcwu),
                  ("VectorWord",     vwdpc,   avgvecwu),
                  ("VectorSliceWord",vswdpc,  avgvswu),
                  ("ArraySliceWord", aswdpc,  avgaswu)]
   fun ranking ((_,p,_),(_,p',_)) = Int.compare (ratToInt p',ratToInt p)
   val labelwidth = 15
   val numberswidth = 7
   val pcswidth = 4
   val bchartwidth = 50
   fun padnum s = UTF8.padLeft #" " numberswidth s
   fun padpc s = UTF8.padLeft #" " pcswidth s
   fun padtname s = UTF8.padRight #" " labelwidth s
   fun barchart pc =
       let val ndots = pc * bchartwidth div 100 - 1
           val nspcs = Int.min(bchartwidth - ndots, 0)
       in UTF8.padRight #" " nspcs (UTF8.padLeft #"." ndots "+")
       end
   fun printtime (s,p,q) =
       print ("  "^(padtname s)^" : "^(padnum(ratToIntString q))^" "^
                                      (padpc(ratToIntString p))^"%. "^
                                      (barchart (ratToInt p))^"\n")
   fun printtimes l = List.app printtime (Listsort.sort ranking l)
in
   val () = print ("  "^(padtname "Representation")^" : "^(padnum("usr μs"))^". "^
                                      (padpc("Prf"))^". (relative to RefWord).\n")
   val () = print (UTF8.padLeft #"-" 87 "\n")
   val _ = printtimes timeres
   val () = print "\n"
end;

val _ = Meta.quietdec := false;

(* To see reference behaviours *)

(*
structure Word8Format =
   WordFormat(structure Word = Word8);
val _ = Meta.installPP Word8Format.pp;

structure WordFormat =
   WordFormat(structure Word = Word);
val _ = Meta.installPP WordFormat.pp;

structure VectorWordFormat =
   WordFormat(structure Word = VectorWord);
val _ = Meta.installPP VectorWordFormat.pp;

structure VectorSliceWordFormat =
   WordFormat(structure Word = VectorSliceWord);
val _ = Meta.installPP VectorSliceWordFormat.pp;

structure ArraySliceWordFormat =
   WordFormat(structure Word = ArraySliceWord);
val _ = Meta.installPP ArraySliceWordFormat.pp;

val two = VectorWord.fromInt 2;
val two' = VectorWord.fromInt 2;
val true = two = two';
val twoslc = VectorWordEnc.toWord8ArraySlice two;
val vl = Word8ArraySlice.foldr (fn (w,a) => w::a) [] twoslc;

val astwo = ArraySliceWord.fromInt 2;
val astwo' = ArraySliceWord.fromInt 2;
val false = astwo = astwo';
val astwoslc = ArraySliceWordEnc.toWord8ArraySlice astwo;
val asl = Word8ArraySlice.foldr (fn (w,a) => w::a) [] astwoslc;

val vstwo = VectorSliceWord.fromInt 2;
val vstwo' = VectorSliceWord.fromInt 2;
val true = vstwo = vstwo';
val vstwoslc = VectorSliceWordEnc.toWord8ArraySlice vstwo;
val vsl = Word8ArraySlice.foldr (fn (w,a) => w::a) [] vstwoslc;

val astwo' = ArraySliceWordEnc.fromWord8ArraySlice vstwoslc;
val astwo'' = ArraySliceWordEnc.fromWord8ArraySlice vstwoslc;

val 2 = VectorSliceWord.toInt vstwo;
val 2 = ArraySliceWord.toInt astwo';
val _ = Word8ArraySlice.update (vstwoslc,0,0w3);
val 3 = ArraySliceWord.toInt astwo';
val 3 = ArraySliceWord.toInt astwo'';
val _ = Word8ArraySlice.update (vstwoslc,0,0w4);
val 4 = ArraySliceWord.toInt astwo';
val 4 = ArraySliceWord.toInt astwo'';
val true = astwo' = astwo'';
val 2 = VectorSliceWord.toInt vstwo;
*)

(* This gives, e.g.:

> val vstwo = 0wx00000002 : word/1
> val vstwo' = 0wx00000002 : word/1
  ...
> val astwo' = 0wx00000002 : word/1
> val astwo'' = 0wx00000002 : word/1

Which makes vstwo and astwo' look like they are the same type. 
But if I type, say:

- vstwo = astwo';

I get this:

! Toplevel input:
! vstwo = astwo';
!         ^^^^^^
! Type clash: expression of type
!   word/1
! cannot have type
!   word/2

So they're not the same, which is good! However I can't figure out why
this is so. I'm not even sure it's a bug :-) I don't think it is
caused by the pretty-printers because it happens when they're not
installed.
 
*)

val () = Ffi.ffi_report_alloc "end of testmcword.sml";

val _ = Meta.quit();
