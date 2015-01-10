val _ = List.app load
   ["MemDebug",    "Vector",               "PolyRedBlackMap",
    "Array",       "ArraySlice",           "RealRepr",
    "SplitFifo",   "GenericArray",         "GenericArraySlice",
    "ObjRepr",     "MappedNativeRegister"];

val _ = MemDebug.debug_set true;

structure Fifo =
  SplitFifo
    (structure WordStruct =
     struct
        type largeword = Word.word
        open Word
     end
     structure ArrayStruct = MappedWord8Array
     structure ArraySliceStruct = MappedWord8ArraySlice
     val length = Word8Vector.length
     val appi = Word8Vector.appi
     val get_cptr = MappedWord8Array.get_cptr
     val zero = 0w0)

val dumpFifo = Fifo.dump

structure ObjRepr =
   ObjRepr
     (type state = Fifo.fifo
      val readByte = Fifo.readByte
      val writeByte = Fifo.writeByte)

fun testCodec (buff : Fifo.fifo) (obj : 'a) : 'a =
   let val _ = ObjRepr.encode (obj,buff)
       val _ = dumpFifo buff 
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

val size = 2048
val nregs = 2
val wbytes = (Word.wordSize + 7) div 8

val arr = MappedWord8Array.array(size+nregs*wbytes,0w0);
val slc = MappedWord8ArraySlice.slice (arr,0,SOME size);
val regsslc = MappedWord8ArraySlice.slice (arr,size,SOME (nregs*wbytes));
val regsv = mkRegisterVector 2 (regsslc);
val regs = (Vector.sub (regsv,0),Vector.sub (regsv,1));

val buff = Fifo.fromSlice (slc,regs);

val ("abcdefg",42.0,0wx7fffffff) = testCodec buff ("abcdefg",42.0,0wx7fffffff);

exception Event of int * string

val e1 = Event (42,"boojum");

val e2 = testCodec buff e1;

val teste = (fn (e) => (if false then (0,"nought") else raise e) handle Event (i,s) => (i,s));

val (42,"boojum") = teste e1

(* Doesn't work with the reconstructed e2. I think because exceptions
   aren't on the heap, they're pointers into a table in the runtime. *)

val (readr,writer) = regs;
val writerv = MappedNativeWordRegister.! writer
val readrv = MappedNativeWordRegister.! readr;

val regsbytes = MappedWord8ArraySlice.foldr op :: [] regsslc;

val _ = dumpFifo buff;

val _ = MemDebug.gc_full_major();

val _ = MemDebug.report_alloc("testing");

val _ = dumpFifo buff;

structure RBMap =
struct
 open PolyRedBlackMap
end

fun addList map l =
   let fun updfn (i,(m,j)) =
         (RBMap.insert
           (m,i,String.str (Char.chr (Char.ord #"q" + j))),j+1)
   in #1(List.foldl updfn (map,0) l)
   end

type dict = (int,string) RBMap.dict

val m : dict = RBMap.mkDict();
val t1 = addList m [2,3,5,7,11,13,17];
val t1l = RBMap.listItems(t1);

fun iter 0 = ()
  | iter n = 
      let val t2 = testCodec buff t1
          val _ = if RBMap.listItems(t2) = t1l
                     then () 
                     else raise Fail "iter: fails"
      in iter (n-1)
      end

val _ = iter 260;

val _ = dumpFifo buff;

datatype 'a mlist =
   Nil 
 | Cons of 'a * 'a mlist ref

fun chgr (Cons (_, r)) l = r := l
  | chgr _ _ = raise Fail "can't change Nil";

val r1 = Cons (0,ref Nil);
val r2 = Cons (1,ref r1);
val r3 = Cons (2,ref r2);

val _ = chgr r1 r1;
val true = (fn (Cons (_,r)) => !r = r1
             | _ => false) r1;

val r1' = Cons (0,ref Nil);
val r2' = Cons (1,ref r1');
val r3' = Cons (2,ref r2');
val r4' = Cons (3,ref r3');
val _ = chgr r1' r4';

val r1'' = testCodec buff r1'
val r2'' = testCodec buff r2'
val r3'' = testCodec buff r3'
val r4'' = testCodec buff r4'

datatype  mtree =
   Leaf 
 | Branch of mtree ref *  mtree ref

fun chg1 (Branch (r, _)) t = r := t
  | chg1 _ _ = raise Fail "can't change Leaf";

fun chg2 (Branch (_, r)) t = r := t
  | chg2 _ _ = raise Fail "can't change Leaf";

val t1 = Branch (ref Leaf,ref Leaf);
val t2 = Branch (ref t1,ref Leaf);
val t3 = Branch (ref t2,ref t2);
val t4 = Branch (ref t3,ref t1);

val t1r  = ObjRepr.valAbsRepr t1
val t1' : mtree = ObjRepr.absReprVal t1r;

val t2r  = ObjRepr.valAbsRepr t2
val t2' : mtree = ObjRepr.absReprVal t2r;

val _ = chg1 t1 t1;
val _ = chg2 t1 t1;

val true = (fn (Branch (r,r')) => !r = t1 andalso !r' = t1
             | _ => false) t1;

val t1' = Branch (ref Leaf,ref Leaf);
val t2' = Branch (ref t1',ref t1');
val t3' = Branch (ref t2',ref t2');
val t4' = Branch (ref t3',ref t3');

val _ = chg1 t1' t1';
val _ = chg2 t1' t1';

val ttuple = ("abcdefg",42.0,0wx7fffffff)
val ttuple2 = ObjRepr.valAbsRepr ttuple
val ttuple' : (string * real * word)
      = ObjRepr.absReprVal ttuple2;

val t1'r = ObjRepr.valAbsRepr t1';
val t1'' : mtree = ObjRepr.absReprVal t1'r;
val t1''r = ObjRepr.valAbsRepr t1'';
val isit1 = t1'r = t1''r;

val t2'r = ObjRepr.valAbsRepr t2';
val t2'' : mtree = ObjRepr.absReprVal t2'r;
val t2''r = ObjRepr.valAbsRepr t2'';
val isit2 = t2'r = t2''r;

val t3'r = ObjRepr.valAbsRepr t3';
val t3'' : mtree = ObjRepr.absReprVal t3'r;
val t3''r = ObjRepr.valAbsRepr t3'';
val isit3 = t3'r = t3''r;

val t4'r = ObjRepr.valAbsRepr t4';
val t4'' : mtree = ObjRepr.absReprVal t4'r;
val t4''r = ObjRepr.valAbsRepr t4'';
val isit4 = t4'r = t4''r;

val t1'' = testCodec buff t1';
val t2'' = testCodec buff t2';
val t3'' = testCodec buff t3';
val t4'' = testCodec buff t4';

val isit = [isit1,isit2,isit3,isit4];

open ObjRepr

val t = compare (1,1);
val t = compare (1,2);
val t = compare (2,1);
val t = compare (0w2,0w1);
val t = compare (0w1,0w1);
val t = compare (0w1,0w2);
val t = compare ("abc","abc");
val t = compare ("abd","abc");
val t = compare ("abc","abd");
val t = compare (42.0,42.0);
val t = compare (42.0,42.1);
val t = compare (42.2,42.0);
val t = compare ((42.2,42.0),(42.2,42.0));
val t = compare ((42.2,42.0),(42.2,42.1));
val t = compare ((42.2,42.1),(42.2,42.0));
val t = compare ((42.9,42.9),(42.9,42.9));
val t = compare ((42.9,42.9),(42.9,42.8));
val t = compare ((42.9,42.8),(42.9,42.9));
val t = compare ((42.9,42.9),(42.9,42.8));

val t1'comp = compare (t1',t1');
val t2'comp = compare (t2',t2');
val t3'comp = compare (t3',t3');
val t4'comp = compare (t4',t4');

val t12'comp = compare (t1',t2');
val t21'comp = compare (t2',t1');
val t34'comp = compare (t3',t4');
val t43'comp = compare (t4',t3');

val (a,b) = (valAbsRepr (42.9,42.9),valAbsRepr (42.9,42.9));
val (a',b') = (valAbsRepr (42.9,42.8), valAbsRepr (42.9,42.9));

fun addList map l =
   let fun updfn (i,(m,j)) =
         (RBMap.insert
           (m,i,String.str (Char.chr (Char.ord #"q" + j))),j+1)
   in #1(List.foldl updfn (map,0) l)
   end

type dict2 = (mtree,string) RBMap.dict

val zoo : dict2 = RBMap.mkDict();

fun iter 0 = ()
  | iter n = 
      let val zoo2 = testCodec buff zoo
      in if compare (zoo2,zoo) = EQUAL
            then () 
            else raise Fail "iter: fails";
         iter (n-1)
      end

val zoo = addList zoo [t1',t2',t3',t4'];
val lvs = List.map #2 (RBMap.listItems(zoo));

val _ = iter 120;

val _ = MemDebug.report_alloc("testing");
