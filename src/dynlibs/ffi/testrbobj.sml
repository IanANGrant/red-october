val _ = load "Redblackmap";
val _ = load "Obj";
val _ = load "ValRepr";
val _ = load "MemDebug";

open ObjRepr

val _ = ArrayTable.clearrefs();

val srep = Obj.repr "abcdefgh";
val srep0 = Obj.obj_field srep 0;
val srep1 = Obj.obj_field srep 1;
val srep2 = Obj.obj_field srep 2;

val sra0 = ValRepr.obj_addr srep0;
val sra1 = ValRepr.obj_addr srep1;
val sra2 = ValRepr.obj_addr srep2;

val intidxs = List.map ArrayTable.addRefSymbol [1,2,3,4];
val stridxs = List.map ArrayTable.addRefSymbol ["abc","ab","abcdef","abcd"];
val intidxs' = List.map ArrayTable.getRefSymbol [1,2,3,4,5];
val stridxs' = List.map ArrayTable.getRefSymbol ["abcd","abcde","abcdef","abc"];

datatype  mtree =
   Leaf 
 | Branch of mtree ref *  mtree ref

fun get1 (Branch (ref r,_)) = r
  | get1 _ = raise Fail "not a branch";

fun get2 (Branch (_,ref r)) = r
  | get2 _ = raise Fail "not a branch";

fun chg1 (Branch (r, _)) t = r := t
  | chg1 _ _ = raise Fail "can't change Leaf";

fun chg2 (Branch (_, r)) t = r := t
  | chg2 _ _ = raise Fail "can't change Leaf";

fun getobjr (obj : mtree ref) =
   let val orep = Obj.repr obj
   in if Obj.obj_size orep = 1
         then Obj.magic (Obj.obj_field orep 0) : mtree
         else raise Fail "not a branch"
   end

fun getobjf (obj : mtree,n) =
   let val orep = Obj.repr obj
   in if Obj.obj_size orep = 0
         then raise Fail "not a branch"
         else Obj.magic (Obj.obj_field orep n) : mtree ref
   end

val t1 = Branch (ref Leaf,ref Leaf);
val t2 = Branch (ref t1,ref t1);
val t3 = Branch (ref t2,ref t2);
val t4 = Branch (ref t3,ref t3);

val t1' = Branch (ref Leaf,ref Leaf);
val t2' = Branch (ref t1',ref t1');
val t3' = Branch (ref t2',ref t2');
val t4' = Branch (ref t3',ref t3');

val _ = chg1 t1' t1';
val _ = chg2 t1' t1';
   
val _ = ArrayTable.clearrefs();
   
val s1 = ArrayTable.addRefSymbol t1';
val s2 = ArrayTable.addRefSymbol t2';
val s3 = ArrayTable.addRefSymbol t3';
   
val isit1 = ArrayTable.getRefSymbol t1';
val isit2 = ArrayTable.getRefSymbol t2';
val isit3 = ArrayTable.getRefSymbol t3';
val isit4 = ArrayTable.getRefSymbol t4';
val s4 = ArrayTable.addRefSymbol t4';
val isit4' = ArrayTable.getRefSymbol t4';

val isit5 = ArrayTable.getRefSymbol (getobjr (getobjf (getobjr (getobjf (t4',1)),1)));
val isit6 = ArrayTable.getRefSymbol (get1 t4');

val t1r  = objAbsRepr (Obj.repr t1);
val t2r  = objAbsRepr (Obj.repr t2);
val t3r  = objAbsRepr (Obj.repr t3);
val t4r  = objAbsRepr (Obj.repr t4);

val t1r'  = objAbsRepr (Obj.repr t1');
val t2r'  = objAbsRepr (Obj.repr t2');
val t3r'  = objAbsRepr (Obj.repr t3');
val t4r'  = objAbsRepr (Obj.repr t4');

val s1 = ArrayTable.addValSymbol "abcd";
val s1' : string = ArrayTable.getValSymbol s1;
val s2 = ArrayTable.addValSymbol 42.0;
val s2' : real = ArrayTable.getValSymbol s2;

fun I (x:'a) =
  let val (n,r) = objAbsReprCnt x
      val _ = ArrayTable.dprint ("I: used n="^(Int.toString n)^" entries in table\n")
      val _ = MemDebug.gc_full_major()
      val (x' : 'a) = absReprObjCnt (n,r)
      val (n',r') = objAbsReprCnt x'
      val _ = ArrayTable.dprint ("I: used n'="^(Int.toString n')^" entries in table\n")
      val _ = MemDebug.gc_full_major()
  in if r = r'
     then x'
     else (printVal r;print "\n";printVal r';print "\n";
           raise Fail "Representation is not bijective.")
  end

val s1 = I "abc"; 
val s2 = I 0wxfffffe; 
val s3 = I 42; 
val s4 = I 42.0; 
val s5 = I (42.0,0wx2a,"forty-two");
val s6 = I (42.0,(0wx2a,"forty-two"));
val s7 = I (ref (42,"forty-two"));

datatype 'a loop =
    Oops
  | Loop of 'a * 'a loop ref

val fix = fn (Loop (_,lr), l) => lr := l 
           | _ => raise Fail "Oops!"

val x = Loop ("x",ref Oops);
val y = Loop ("y",ref Oops);

val s8 = I x;
val _ = fix (x,y);
val s9 = I x;
val _ = fix (y,x);
val s10 = I x;

val s11 = I t4;
val s12 = I t4';

