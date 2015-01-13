
 (* Abstract representation of values

    Atoms (unit,nil,true,false etc.) are 0-tuples, refs are 1-tuples, etc. the
    tag gives the type (which is always either ref, or a vector, or an atom.)
    Reals, strings, Word8Vectors are all ByteVectors, the tag gives the type.
    Words, ints etc are Words.

    Decl declares a symbol, Ref refers to a previous declaration by
    its index. All N symbols comprising any representation of a
    _whole_ object should have consecutive indices 0,1,...,N-1, with
    all references declared before they are used. Then a serial
    representation of the object encodes all declarations before
    references, so that reconstruction proceeds bottom-up and can
    therefore be done in a single forward pass without backpatching or
    anything "clever" like that.
 *)

(* The following structure is no longer used. It was the
   prototype. It's here for the sake of posterity. *)

structure RedBlackTable =
struct
   local
      fun newdicts () =
            (Redblackmap.mkDict (Int.compare),
             Redblackmap.mkDict (Int.compare))
      val (dictref,dictobj) : (int, Obj.obj ref) Redblackmap.dict ref *
                              (int, Obj.obj) Redblackmap.dict ref =
                let val (d1,d2) = (newdicts())
                in (ref d1,ref d2) end
      val nextrefidx = ref 0
      val nextobjidx = ref 0
      fun clearrefs () = (dictref := (Redblackmap.mkDict (Int.compare));
                          nextrefidx := 0)
      fun clearobjs () = (dictobj := (Redblackmap.mkDict (Int.compare));
                          nextobjidx := 0)
      val oref : 'a -> Obj.obj ref = fn x => ref (Obj.repr x)
      fun initobjs (n:int) = clearobjs ()
      fun initrefs (n:int) = clearrefs ()
      fun getnextrefid () =
         !nextrefidx before nextrefidx := (!nextrefidx) + 1
      fun getnextobjid () =
         !nextobjidx before nextobjidx := (!nextobjidx) + 1
      fun addref x =
         let val or = oref x
             val newid = getnextrefid ()
         in dictref := (Redblackmap.insert (!dictref,newid,or));
            newid         
         end
      fun addobj obj =
         let val newid = getnextobjid ()
         in dictobj := (Redblackmap.insert (!dictobj,newid,obj));
            newid         
         end
      fun addobjidx (n,obj) =
         dictobj := (Redblackmap.insert (!dictobj,n,obj))
      fun objcount () = Redblackmap.numItems (!dictobj)
      fun refcount () = Redblackmap.numItems (!dictref)
      exception Found of int
   in
      val clearrefs = clearrefs
      val clearobjs = clearobjs
      val initrefs = initrefs
      val initobjs = initobjs
      val objcount = objcount
      val refcount = refcount
      fun getRefSymbol x =
         let val or = Obj.repr x
             fun foldfn (idx,ref obj,NONE) =
                    if obj = or
                       then raise Found idx
                       else NONE
               | foldfn (_,_,r as (SOME _)) = r
         in Redblackmap.foldl foldfn NONE (!dictref) handle Found n => SOME n
         end
      fun addRefSymbol x =
         case getRefSymbol x
           of SOME n => n
            | NONE => addref x
      fun addObjSymbol (n,x) = addobjidx (n,x)
      fun getObjSymbol n =
         Redblackmap.find (!dictobj,n)
      fun getValSymbol n =
         Obj.magic (Redblackmap.find (!dictobj,n))
      fun addValSymbol x = addobj (Obj.repr x)
   end
end

 (* This badly needs a rewrite: split out the two pre-allocated block
    arrays as a separate unit. I haven't done this because I want a
    more general slab-allocator with a closure stack that works on
    MappedWord8Array.array as well.*)

structure ArrayTable =
struct
   val debugging = ref false
   fun dprint s =
      if !debugging then print s else ()
   local
      val blksz = ref 1024
      val nblks = ref 256
      fun mkrefblk n = Array.array(n,ref (Obj.repr ()))
      fun mkobjblk n = Array.array(n,Obj.repr ())
      fun inittblref (nblks) =
            Array.tabulate
              (nblks,fn i => if i = 0
                                then mkrefblk (!blksz)
                                else mkrefblk 0)
      fun inittblobj (nblks) =
            Array.tabulate
              (nblks,fn i => if i = 0
                                then mkobjblk (!blksz)
                                else mkobjblk 0)
      val tblref = inittblref (!nblks)
      val tblobj = inittblobj (!nblks)
      fun cleartblref (n) = 
             (blksz := n;
              Array.modifyi
                   (fn (i,_) => if i = 0
                                   then mkrefblk n
                                   else mkrefblk 0) tblref)
      fun cleartblobj (n) =
             (blksz := n;
              Array.modifyi
                  (fn (i,_) =>
                     if i = 0
                        then mkobjblk n
                        else mkobjblk 0) tblobj)
      val nextrefidx = ref 0
      val nextobjidx = ref 0
      fun clearrefs (n) = (cleartblref (n);
                           nextrefidx := 0)
      fun clearobjs (n) = (cleartblobj (n);
                           nextobjidx := 0)
      fun updatetbl mkblk updnxtidx =
       fn (tbl,n,x) =>
         let val _ = dprint ("updatetebl: "^(Int.toString n)^" blksz: "^(Int.toString (!blksz))^"\n")
             val idx = n div (!blksz)
             val offs =  n mod (!blksz)
             val _ = dprint ("   index: "^(Int.toString idx)^"\n")
             val _ = dprint ("    offs: "^(Int.toString offs)^"\n")
             val blk = let val blk = Array.sub(tbl,idx)
                       in if Array.length (blk) = 0
                             then let val blk = mkblk (!blksz)
                                  in Array.update(tbl,idx,blk);
                                     blk
                                  end
                          else blk
                       end
             in Array.update(blk,offs,x);
                updnxtidx (n+1)
             end
      fun updatenextobjidx n =
           let val _ = if n > (!nextobjidx) then nextobjidx := n else ()
               val _ = (dprint ("objcount: updatenextobjidx = "^
                          (Int.toString (!nextobjidx))^"\n");!nextobjidx)
           in () end
      fun updatenextrefidx n =
           let val _ = if n > (!nextrefidx) then nextrefidx := n else ()
               val _ = (dprint ("refcount: updatenextrefidx = "^
                          (Int.toString (!nextrefidx))^"\n");!nextrefidx)
           in () end
      val updatereftbl = updatetbl mkrefblk updatenextrefidx
      val updateobjtbl = updatetbl mkobjblk updatenextobjidx
      fun subtbl (tbl,n) =
         let val _ = dprint ("sub: "^(Int.toString n)^" blksz: "^(Int.toString (!blksz))^"\n")
             val idx = n div (!blksz)
             val offs = n mod (!blksz)
             val _ = dprint ("   index: "^(Int.toString idx)^"\n")
             val _ = dprint ("    offs: "^(Int.toString offs)^"\n")
             val blk = Array.sub(tbl,idx)
             in Array.sub(blk,offs)
             end
      val oref : 'a -> Obj.obj ref = fn x => ref (Obj.repr x)
      fun initobjs (n) = clearobjs (n)
      fun initrefs (n) = clearrefs (n)
      fun getnextrefid () =
         let val nextidx = !nextrefidx
         in nextrefidx := nextidx + 1;
            nextidx
         end
      fun getnextobjid () =
         let val nextidx = !nextobjidx
         in nextobjidx := nextidx + 1;
            nextidx
         end
      fun addref x =
         let val or = oref x
             val newid = getnextrefid ()
         in updatereftbl (tblref,newid,or);
            newid
         end
      fun addobj obj =
         let val newid = getnextobjid ()
         in updateobjtbl (tblobj,newid,obj);
            newid
         end
      fun addobjidx (n,obj) =
         (updateobjtbl (tblobj,n,obj);
          updatenextobjidx n)
      fun refcount () = (dprint ("refcount: nextrefidx = "^(Int.toString (!nextrefidx))^"\n");!nextrefidx)
      fun objcount () = (dprint ("objcount: nextobjidx = "^(Int.toString (!nextobjidx))^"\n");!nextobjidx)
      fun foldr nextidx f a =
        fn tbl =>
           let fun iter a 0 = a
                 | iter a n =
                    let val _ = dprint ("fold: iter: n="^(Int.toString n)^"\n")
                        val n' = n - 1
                    in iter (f (n',subtbl (tbl, n'),a)) n'
                    end
           in iter a nextidx
           end
      exception Found of int
   in
      val initrefs = clearrefs
      val initobjs = clearobjs
      val clearrefs = fn () => clearrefs (1024)
      val clearobjs =  fn () => clearobjs (1024)
      val objcount = objcount
      val refcount = refcount
      fun getRefSymbol x =
         let val or = Obj.repr x
             fun foldfn (idx,ref obj,NONE) =
                    if obj = or
                       then raise Found idx
                       else NONE
               | foldfn (_,_,r as (SOME _)) = r
         in foldr (refcount()) foldfn NONE tblref handle Found n => SOME n
         end
      fun addRefSymbol x =
         case getRefSymbol x
           of SOME n => n
            | NONE => addref x
      fun addObjSymbol (n,x) = addobjidx (n,x)
      fun getObjSymbol n =
         subtbl (tblobj,n)
      fun getValSymbol n =
         Obj.magic (subtbl (tblobj,n))
      fun addValSymbol x = addobj (Obj.repr x)
   end
end

structure AbsObjRepr =
struct
   datatype rep =
       Tuple of int * sym Vector.vector        (* Tuple(tag,elts) *)
     | ByteVector of int * Word8Vector.vector  (* ByteVector(tag,bytes) *)
     | Word of word
   and sym =
       Decl of int * rep                       (* Decl(idx,rep) *)
     | Ref of int                              (* Ref(idx) *)
   val debugging = ref false
   local open ArrayTable
      fun dprint s =
         if !debugging then print s else ()
      local
         fun addWord w =
            case getRefSymbol w
              of NONE => Decl (addRefSymbol w,Word w)
               | SOME n => Ref n
         fun addBytes x =
            let val obj = Obj.repr x
            in case getRefSymbol x
                 of NONE => Decl (addRefSymbol x,
                                  ByteVector(Obj.obj_tag obj,
                                             ValRepr.objWord8Vector obj))
                  | SOME n => Ref n
            end
         fun addTuple x =
            case getRefSymbol x
              of NONE =>
                   let val idx = addRefSymbol x
                       val obj = Obj.repr x
                       val tag = Obj.obj_tag obj
                       val len = Obj.obj_size obj
                       val tabfn = addObj o (Obj.obj_field obj)
                       val vec = Vector.tabulate (len,tabfn)
                   in Decl (idx, Tuple(tag, vec))
                   end
               | SOME n => Ref n
         and addObj (obj : Obj.obj) =
            if (ValRepr.obj_addr obj) mod 0w2 = 0w1
               then addWord (Obj.magic obj)
               else if (Obj.obj_tag obj) < ValRepr.no_scan_tag
                       then addTuple (Obj.magic obj)
                       else addBytes (Obj.magic obj)
         fun objAbsRepr x =
            let val _ = initrefs (1024)
            in addObj (Obj.repr x)
            end
      in val objAbsRepr = objAbsRepr
         fun objAbsReprCnt x =
            let val repr = objAbsRepr x
                val cnt = refcount()
            in (cnt,repr)
            end
      end
      local
         fun mkWord (idx,w) =
            let val obj = Obj.repr w
                val _ = addObjSymbol (idx,obj)
            in obj
            end
         fun mkByteVector (idx,p) =
            let val obj = ValRepr.word8VectorObj p
                val _ = addObjSymbol (idx,obj)
            in obj
            end
         fun mkTuple (idx,(tag,vec)) =
            let val obj = Obj.obj_block tag (Vector.length vec)
                val _ = addObjSymbol (idx,obj)
                fun appifn (i,x) = Obj.set_obj_field obj i (mkSym x)
            in Vector.appi appifn vec;
               obj
            end
         and mkSym (Decl (n,Word w)) = mkWord (n,w)
           | mkSym (Decl (n,ByteVector p)) = mkByteVector (n,p)
           | mkSym (Decl (n,Tuple p)) = mkTuple (n,p)
           | mkSym (Ref n) = getObjSymbol n
         fun absReprObj rep =
            let val _ = initobjs (1024)
            in Obj.magic (mkSym rep)
            end
         fun absReprObjCnt (cnt,rep) =
            let val _ = initobjs (cnt)
            in Obj.magic (mkSym rep)
            end
      in val absReprObj = absReprObj
         val absReprObjCnt = absReprObjCnt
      end
   in val objAbsRepr = objAbsRepr
      val absReprObj = absReprObj
      val objAbsReprCnt = objAbsReprCnt
      val absReprObjCnt = absReprObjCnt
   end
   local open ArrayTable
      local
         fun addWord w =
            case getRefSymbol w
              of NONE => Decl (addRefSymbol w,Word w)
               | SOME n => Ref n
         fun addBytes x =
            let val obj = Obj.repr x
            in case getRefSymbol x
                 of NONE => Decl (addRefSymbol x,
                                  ByteVector(Obj.obj_tag obj,
                                             ValRepr.objWord8Vector obj))
                  | SOME n => Ref n
            end
         fun addTuple x =
            case getRefSymbol x
              of NONE =>
                   let val idx = addRefSymbol x
                       val obj = Obj.repr x
                       val tag = Obj.obj_tag obj
                       val len = Obj.obj_size obj
                       val tabfn = addObj o (Obj.obj_field obj)
                       val vec = Vector.tabulate (len,tabfn)
                   in Decl (idx, Tuple(tag, vec))
                   end
               | SOME n => Ref n
         and addObj (obj : Obj.obj) =
            if (ValRepr.obj_addr obj) mod 0w2 = 0w1
               then addWord (Obj.magic obj)
               else if (Obj.obj_tag obj) < ValRepr.no_scan_tag
                       then addTuple (Obj.magic obj)
                       else addBytes (Obj.magic obj)
         fun objAbsRepr x =
            let val _ = clearrefs ()
            in addObj (Obj.repr x)
            end
      in val objAbsRepr = objAbsRepr
         fun objAbsReprCnt x =
             let val repr = objAbsRepr x
                 val cnt = refcount()
             in (cnt,repr)
             end
      end
      local
         fun mkWord (idx,w) =
            let val obj = Obj.repr w
                val _ = addObjSymbol (idx,obj)
            in obj
            end
         fun mkByteVector (idx,p) =
            let val obj = ValRepr.word8VectorObj p
                val _ = addObjSymbol (idx,obj)
            in obj
            end
         fun mkTuple (idx,(tag,vec)) =
            let val obj = Obj.obj_block tag (Vector.length vec)
                val _ = addObjSymbol (idx,obj)
                fun appifn (i,x) = Obj.set_obj_field obj i (mkSym x)
            in Vector.appi appifn vec;
               obj
            end
         and mkSym (Decl (n,Word w)) = mkWord (n,w)
           | mkSym (Decl (n,ByteVector p)) = mkByteVector (n,p)
           | mkSym (Decl (n,Tuple p)) = mkTuple (n,p)
           | mkSym (Ref n) = getObjSymbol n
         fun absReprObj rep =
            let val _ = clearobjs ()
            in Obj.magic (mkSym rep)
            end
         fun absReprObjCnt (cnt,rep) =
            let val _ = initobjs (cnt)
            in Obj.magic (mkSym rep)
            end
      in val absReprObj = absReprObj
         val absReprObjCnt = absReprObjCnt
      end
  fun byteVectorCompare (v,v') =
      let val (len,len') = (Word8Vector.length v,Word8Vector.length v')
          fun iter (0,0) = EQUAL
            | iter (0,_) = LESS
            | iter (_,0) = GREATER
            | iter (i,i') = 
                 let val (n,n') = (len -i,len'-i')
                     val (b,b') = (Word8Vector.sub (v,n),Word8Vector.sub (v',n'))
                     val r' = Word8.compare (b,b')
                 in if r' = EQUAL then iter (i-1,i'-1) else r'
                 end
      in iter (len,len')
      end
  fun compare_sym (Decl (n,r),Decl (n',r')) =
        if n = n'
           then compare_rep (r,r')
           else Int.compare (n,n')
    | compare_sym (Ref n,Ref n') =
           Int.compare (n,n')

   (* The following two cases are required because the abstract
      representation uses Ref's to abbreviate "common subexpressions"
      This scheme "works" but the order it determines is random. We
      could implement a version which puts the expected order on any
      structure, but it would be a bit slower. All we need to do is to
      keep a record of which Ref's we are under (i.e. ones we are
      currently deciding) and then we would "go under" a ref only if
      it wasn't in the list, or if the other side hadn't gone under
      its corresponding Ref, if it has one. Whenever we arrive at one
      of these "no go" Ref's, we would have to decide the result of
      the comparison. This might be contradictory: a < b <=> a > b, in
      which case we would just say they are equal (because if they
      were unequal there would be a contradiction, so by
      reductio-ad-absurdum, they must be equal. There is also the
      undertermined case: a < b <=> b > a && a > b <=> b < a. And in
      this case too we could simply choose equality without risk of
      introducing inconsistency. (c.f. Euclid's definition of
      proportion as explained by Charles Dodgson in the Appendix to
      Euclid Book V, proved Algebraically as far as rationals : that
      two measures are proportional if, for any common multiple
      whatsoever, ...)

      In order to be able to do this, we would need to carry around an
      associative memory of some kind which we can use to quickly
      check whether we are 'under' any given reference, and which
      would also give us the bijection between the primitive values we
      have seen so far. *)

    | compare_sym (Ref n,Decl (n',r')) =
           Int.compare (n,n')
    | compare_sym (Decl (n,r),Ref n') =
           Int.compare (n,n')
  and compare_rep (Word w,Word w') =
           Word.compare (w,w')
    | compare_rep (ByteVector(t,v),ByteVector(t',v')) =
           let val r = Int.compare (t,t')
           in if r = EQUAL
                 then byteVectorCompare (v,v')
                 else r
           end
    | compare_rep (Tuple(t,v),Tuple(t',v')) =
           let val r = Int.compare (t,t')
           in if r = EQUAL
                 then vectorCompare (v,v')
                 else r
           end
    | compare_rep _ = raise Fail "ObjRepr.compare_sym: internal error"
  and vectorCompare (v,v') =
      let val (len,len') = (Vector.length v,Vector.length v')
          fun iter (0,0) = EQUAL
            | iter (0,_) = LESS
            | iter (_,0) = GREATER
            | iter (i,i') = 
                 let val (n,n') = (len -i,len'-i')
                     val (s,s') = (Vector.sub (v,n),Vector.sub (v',n'))
                     val r' = compare_sym (s,s')
                 in if r' = EQUAL then iter (i-1,i'-1) else r'
                 end
      in iter (len,len')
      end
   in val objAbsRepr = objAbsRepr
      val absReprObj = absReprObj
      val objAbsReprCnt = objAbsReprCnt
      val absReprObjCnt = absReprObjCnt
      val compare_abs = compare_sym
      fun dprint s =
         if !debugging then print s else ()
   end
end
