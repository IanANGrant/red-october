(* Values.sml - a wrapper for the Obj unit with names that are
   hopefully easier to remember, and with a little more error
   checking. Except for typing errors in the value call, this thing
   shouldn't segfault. It would be nice to have type introspection
   too, and then we could check even the magic calls. *)

structure Values :> Values =
struct
   datatype tag =
           Double
         | Word
         | Address
         | Closure
         | Bytes
         | Abstract
         | Finalized
         | Weak
         | Ref
         | Atom of int
         | Tuple of int
      type valrep = Obj.obj
      exception Repr
   local
      val wordBytes = (Word.wordSize + 7) div 8
      val dlh = Dynlib.dlopen
                      {lib = "libcinfo.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }
      val (num_tags,no_scan_tag,weak_tag,string_tag,ref_tag,
           double_tag,abstract_tag,final_tag,closure_tag)
          : int * int * int * int * int * int * int * int * int  = 
                Dynlib.app1 (Dynlib.dlsym dlh "heap_constants") ();
      val Is_in_heap : 'a -> bool =
          fn obj =>
            (MemDebug.gc_minor(); (* I don't know why, but Is_in_heap doesn't see it without this. *)
             Dynlib.app1 (Dynlib.dlsym dlh "heap_Is_in_heap") obj);
      val Is_atom : 'a -> bool = fn obj => Dynlib.app1 (Dynlib.dlsym dlh "heap_Is_atom") obj;
      val atom : int -> valrep = fn n => Dynlib.app1 (Dynlib.dlsym dlh "heap_Atom") n;
      val tags = [(double_tag,Double),
                  (~1,Word),
                  (~2,Address),
                  (closure_tag,Closure),
                  (string_tag,Bytes),
                  (abstract_tag,Abstract),
                  (final_tag,Finalized),
                  (weak_tag,Weak),
                  (ref_tag,Ref)];
      fun fromInt n =
         let fun lookup n =
                List.find (fn (i,_) => i = n) tags
         in case lookup n
              of NONE => if n < final_tag
                            then Tuple n
                            else raise Repr
               | SOME (_,t) => t
         end
      fun toInt tag =
           case tag
             of Double => double_tag
              | Word => ~1
              | Address => ~2
              | Tuple n => n
              | Closure => closure_tag
              | Bytes => string_tag
              | Abstract => abstract_tag
              | Finalized => final_tag
              | Weak => weak_tag
              | Ref => ref_tag
              | Atom i => i
      fun objWord8Vector obj =
         let val len = Obj.obj_size obj
             val nbytes = wordBytes * len
             val vec = Word8Vector.tabulate (nbytes,fn _ => 0w0)
             val vecrepr = Obj.repr vec
             val oflds = Vector.tabulate (len,Obj.obj_field obj)
             val () = Vector.appi (fn (i,f) => (Obj.set_obj_field vecrepr i f)) oflds
         in vec
         end
      val littleendian = (* We check the little end, so this should work on 64 bit too *)
         let val ev = objWord8Vector (Obj.repr (Vector.tabulate (1,fn _ => 0wx4321)))
         in Word8Vector.sub(ev,0) = 0wx21 * 0w2 + 0w1 (* Long_val conversion *)
         end
      fun toWord (obj : Obj.obj) : Word.word =
         let val vec = objWord8Vector (Obj.repr (Vector.tabulate (1,fn _ => obj)))
             val len = Word8Vector.length vec
             val fold = if littleendian
                           then Word8Vector.foldr
                           else Word8Vector.foldl
             val result = fold (fn (w8,w) => Word.orb(Word.<<(w,0w8),Word8.toLarge w8)) 0w0 vec
         in result
         end
   in
      val cptr : 'a -> Dynlib.cptr = Obj.magic o Obj.repr
      val repr : 'a -> valrep = Obj.repr
      val value : valrep -> 'a = Obj.magic_obj
      val new : tag * int -> valrep =
            fn (t',i) =>
             case t'
               of Atom n => if i = 0
                               then atom n
                               else raise Fail "Values.new: atom length nonzero"
                | _ => let val t = toInt t'
                       in if t < 0
                          then raise Repr
                          else Obj.obj_block t i
                       end
      val tag : 'a -> tag = 
            fn x =>
               if Word.andb(toWord (Obj.repr x),0wx1) = 0wx1
                     then Word
                     else if Is_in_heap x
                             then if Obj.obj_size (Obj.repr x) = 0
                                     then fromInt (Obj.obj_tag (Obj.repr x))
                                     else if Obj.obj_tag (Obj.repr x) = 0
                                             then Tuple 0
                                             else fromInt (Obj.obj_tag (Obj.repr x))
                             else if Is_atom x
                                     then if Obj.obj_size (Obj.repr x) = 0
                                             then Atom (Obj.obj_tag (Obj.repr x))
                                             else Tuple (Obj.obj_tag (Obj.repr x))
                                     else Address
      val length : 'a -> int =
          fn x => if Word.andb(toWord (Obj.repr x),0wx1) = 0wx1
                     then 0
                     else if Is_in_heap x
                             then Obj.obj_size (Obj.repr x)
                             else 0
      val dumpb : 'a -> unit =
            fn x =>
               let val len = length x * wordBytes
               in if len = 0
                     then ()
                     else MemDebug.dumpb (cptr x) len 
               end
      val dumpw : 'a -> unit =
            fn x =>
               let val len = length x
               in if len = 0
                     then ()
                     else MemDebug.dumpw (cptr x) len 
               end
      val sub : 'a * int -> valrep =
         fn (x,i) =>
           if i < length x
              then Obj.obj_field (Obj.repr x) i
              else raise Subscript
      val update : 'a * int * 'b -> unit =
         fn (x,i,x') =>
           if i < length x
              then Obj.set_obj_field (Obj.repr x) i (Obj.repr x')
              else raise Subscript
   end
end
