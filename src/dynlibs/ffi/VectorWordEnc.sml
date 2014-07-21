functor VectorWordEnc(structure PrimEnc : PrimEnc 
                      structure Word : WordPrim)
    :> WordEnc where type excn = PrimEnc.excn =
struct
   local
       prim_eqtype vector_
       prim_val vector_  : int -> vector_                       = 1 "create_string"
       prim_val fill_    : vector_ -> int -> int -> int -> unit = 4 "fill_string"

       prim_val mkvec_  : int -> 'x -> vector_ vector              = 2 "make_vect";
       prim_val sub_    : vector_ vector -> int -> vector_         = 2 "get_vect_item";
       prim_val update_ : vector_ vector -> int -> vector_ -> unit = 3 "set_vect_item";

       type vector = vector_ vector
       open Jit
   in
      open PrimEnc
      type word = vector
      type largeword = Word.word
      val wordSize = WORDSIZE
      local
         val wsz = WORDSIZE div 8
         prim_val fromWord8Vector_ : Word8Vector.vector -> vector_ = 1 "identity"
         prim_val toWord8Vector_ : vector_ -> Word8Vector.vector = 1 "identity"
         fun wrapVec v' =
             let val v = mkvec_ 1 ()
             in update_ v 0 v';
                v : word
             end
         fun unWrapVec v =
             let val v' = sub_ v 0
             in v'
             end
      in
(*         prim_val toLargeWord   : word -> word = 1 "identity"
         prim_val toLargeWordX  : word -> word = 1 "identity"
         prim_val fromLargeWord : word -> word = 1 "identity"
         prim_val toLarge   : word -> word = 1 "identity"
         prim_val toLargeX  : word -> word = 1 "identity"
         prim_val fromLarge : word -> word = 1 "identity" *)
         val fromWord8VectorSlice = wrapVec o fromWord8Vector_ o Word8VectorSlice.vector
         val toWord8VectorSlice = Word8VectorSlice.full o toWord8Vector_ o unWrapVec
         fun fromWord8ArraySlice slc =
             let val len = Word8ArraySlice.length slc
                 val (a,i,n) = Word8ArraySlice.base slc
             in if len <> wsz 
                   then raise Size
                   else fromWord8VectorSlice (Word8VectorSlice.slice (Word8Array.vector a,i,SOME n))
             end
         fun toWord8ArraySlice w =
             let val vslc = toWord8VectorSlice w
                 val (v,i,n) = Word8VectorSlice.base vslc
                 val a = Word8Array.array(Word8Vector.length v,0w0)
                 val _ = Word8Array.copyVec {src=v,dst=a,di=0}
             in Word8ArraySlice.slice (a,i,SOME n)
             end
         val toLargeWord = Word.fromWord8ArraySlice o toWord8ArraySlice
         val toLargeWordX = Word.fromWord8ArraySlice o toWord8ArraySlice
         val toLarge = Word.fromWord8ArraySlice o toWord8ArraySlice
         val toLargeX = Word.fromWord8ArraySlice o toWord8ArraySlice
         val fromLargeWord = fromWord8ArraySlice o Word.toWord8ArraySlice
         val fromLarge = fromWord8ArraySlice o Word.toWord8ArraySlice
         val toLargeInt = Word.fromWord8ArraySlice o toWord8ArraySlice
         val toLargeIntX = Word.fromWord8ArraySlice o toWord8ArraySlice
         val fromLargeInt = fromWord8ArraySlice o Word.toWord8ArraySlice
         fun mkVal () = (* I don't exactly know how it works, but this extra indirection
                           makes the resulting type, like 'a Vector.vector, an
                           equality type _by values._ The Array 
                           representations of words are only equality types by reference. *)
             let val v = mkvec_ 1 ()
                 val v' = vector_ wsz
             in fill_ v' 0 wsz 0; 
                update_ v 0 v';
                v : word
             end
         fun jit_ldargp0 (jit_, r2, v0, r1, v1) =
            let val _  = jit_ldxi (jit_, r2, r1, 0)       (* r2 = Field(r1,0) *)
                val _  = jit_movi (jit_, v0, 0)           (* v0 = 0 *)
            in ()
            end
         fun jit_ldargp (jit_, r2, v0, r0, r1, v1, n) =
            let val _  = jit_ldxi (jit_, r1, r0, wsz * n) (* r1 = Field(r0,n) *)
                val _  = jit_ldargp0 (jit_, r2, v0, r1, v1)
            in ()
            end
      end
   end
end
