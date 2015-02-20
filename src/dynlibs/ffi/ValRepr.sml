signature ValRepr =
sig
   val wordBytes : int
   val abstract_tag : int
   val ref_tag : int
   val string_tag : int
   val final_tag : int
   val weak_tag : int
   val double_tag : int
   val no_scan_tag : int
   val littleendian : bool
   val wordFromCptr : Dynlib.cptr -> Word.word
   val cptrFromWord : Word.word -> Dynlib.cptr 
   val obj_addr : Obj.obj -> Word.word
   val obj_cptr : Obj.obj -> Dynlib.cptr
   val objBytes : Obj.obj -> word8 Vector.vector
   val word8VectorObj : int * Word8Vector.vector -> Obj.obj
   val objWord8Vector : Obj.obj -> Word8Vector.vector
   val word8VectorWord : Word8Vector.vector -> Word.word
   val wordWord8Vector : Word.word -> Word8Vector.vector
   val word8VectorCptr : Word8Vector.vector -> Dynlib.cptr
   val cptrWord8Vector : Dynlib.cptr -> Word8Vector.vector
   val longFromWord : Word.word -> Dynlib.cptr 
   val wordFromLong : Dynlib.cptr -> Word.word
   val cptr_offs : Dynlib.cptr -> int -> Dynlib.cptr (* Should be in a cptr arithmetic unit *)
end

structure ValRepr :> ValRepr =
struct
   local
   in
      val ref_tag = Obj.obj_tag (Obj.repr (ref 0))
      val wordBytes = (Word.wordSize + 7) div 8
      val no_scan_tag = ref_tag + 1
      val weak_tag = no_scan_tag
      val string_tag = Obj.obj_tag (Obj.repr "")
      val abstract_tag = no_scan_tag + 1 (* Should come from headers/C compiler *)
      val double_tag = no_scan_tag + 3
      val final_tag = no_scan_tag + 4
(* >= no_scan_tag means it is a sequence of bytes,
    < no_scan_tag means it's an n-tuple *)
      fun objWord8Vector obj =
         let val len = Obj.obj_size obj
             val bpw = (Word.wordSize + 7) div 8
             val nbytes = bpw * len
             val vec = Word8Vector.tabulate (nbytes,fn _ => 0w0)
             val vecrepr = Obj.repr vec
             val oflds = Vector.tabulate (len,Obj.obj_field obj)
             val () = Vector.appi (fn (i,f) => (Obj.set_obj_field vecrepr i f)) oflds
         in vec
         end
      val obj_cptr : Obj.obj -> Dynlib.cptr = Obj.magic
      val littleendian =
         let val ev = objWord8Vector (Obj.repr (Vector.tabulate (1,fn _ => 0wx4321)))
         in Word8Vector.sub(ev,0) = 0wx21 * 0w2 + 0w1 (* Long_val conversion *)
         end
      fun wordFromCptr (cptr : Dynlib.cptr) : Word.word =
         let val vec = objWord8Vector (Obj.repr (Vector.tabulate (1,fn _ => cptr)))
             val len = Word8Vector.length vec
             val fold = if littleendian then Word8Vector.foldr else Word8Vector.foldl
             val result = fold (fn (w8,w) => Word.orb(Word.<<(w,0w8),Word8.toLarge w8)) 0w0 vec
         in result
         end
      val obj_addr = wordFromCptr o obj_cptr
      fun wordWord8Vector (w : Word.word) =
             objWord8Vector (Obj.repr (Vector.tabulate (1,fn _ => w)))
      fun cptrWord8Vector (cp : Dynlib.cptr) =
             objWord8Vector (Obj.repr (Vector.tabulate (1,fn _ => cp)))
      fun word8VectorObj (tag,vec) =
         let val len = Word8Vector.length vec
             val bpw = (Word.wordSize + 7) div 8
             val nwords = len div bpw
             val obj = Obj.obj_block tag nwords
             val vecrepr = Obj.repr vec
             fun loop 0 = ()
               | loop n = let val n' = n - 1
                    in Obj.set_obj_field obj n' (Obj.obj_field vecrepr n');
                       loop n'
                    end
         in loop nwords;
            obj
         end
      fun word8VectorWord (v : Word8Vector.vector) : Word.word =
             Obj.magic (Obj.obj_field (Obj.repr v) 0)
      fun word8VectorCptr (v : Word8Vector.vector) : Dynlib.cptr =
             Obj.magic (Obj.obj_field (Obj.repr v) 0)
      val cptrFromWord : Word.word -> Dynlib.cptr = 
          fn w => 
            let val l = (Word.wordSize + 7) div 8
                val indexfn = if littleendian
                                 then (fn i => i)
                                 else (fn i => l - i - 1)
                fun tabulatefn w =
                    fn i => let val b = indexfn i
                            in Word8.fromLarge (Word.andb((Word.>>(w,Word.*(Word.fromInt b,0w8))),0wxff))
                            end
                val vec = Word8Vector.tabulate (l,tabulatefn w)
            in Obj.magic (Obj.obj_field (Obj.repr vec) 0)
            end
      val longFromWord = cptrFromWord
      val wordFromLong = wordFromCptr
      fun objBytes obj =
         let val v = objWord8Vector obj
             val tabfn = fn i => Word8Vector.sub (v,i)
         in (Vector.tabulate (Word8Vector.length v, tabfn)) 
         end
      fun cptr_offs cptr offs =
         let val cpw = wordFromCptr cptr
             val cpw' = cpw + (Word.fromInt offs)
         in cptrFromWord cpw'
         end
   end
end

(*

val sw8a = StaticWord8Array.array (128,0w0);
val sw8a_Addr = ValRepr.wordFromCptr (StaticBuffer.buffer sw8a);
val sw8a_Addr' = StaticBuffer.svec_getcptrword (StaticBuffer.buffer sw8a);

val w8v = ValRepr.wordWord8Vector 0wx4321;
val w8vv = ValRepr.objBytes (Obj.repr w8v);
val 0wx4321 = ValRepr.word8VectorWord w8v;

val obj = Obj.obj_block ValRepr.final_tag 6;

val wvbs = ValRepr.objBytes (Obj.repr (ref (ValRepr.longFromWord 0wx4321ff)));

val I = ValRepr.wordFromCptr o ValRepr.longFromWord;
val 0wx7FFFFFFF = I 0wx7FFFFFFF;

*)
