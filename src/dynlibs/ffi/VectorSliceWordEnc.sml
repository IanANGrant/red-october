functor VectorSliceWordEnc(structure PrimEnc : PrimEnc
                           structure Word : WordPrim) :> WordEnc =
struct
   local
       prim_eqtype vector_
       prim_val vector_  : int -> vector_                       = 1 "create_string"
       prim_val fill_    : vector_ -> int -> int -> int -> unit = 4 "fill_string"
       type vector = vector_
       open Jit
   in
      open PrimEnc
      type word = (vector * int * int)
      type largeword = Word.word
      val wordSize = WORDSIZE
      local
         val wsz = WORDSIZE div 8
         prim_val fromWord8VectorSlice_ : Word8VectorSlice.slice -> word = 1 "identity"
         prim_val toWord8VectorSlice_ : word -> Word8VectorSlice.slice = 1 "identity"
      in
         val fromWord8VectorSlice = fromWord8VectorSlice_
         val toWord8VectorSlice = toWord8VectorSlice_
         fun fromWord8ArraySlice slc =
             let val (a,i,n) = Word8ArraySlice.base slc
             in fromWord8VectorSlice (Word8VectorSlice.slice (Word8Array.vector a,i,SOME n))
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
         fun mkVal () =
             let val v = vector_ wsz
             in fill_ v 0 wsz 0;
                (v, 0, wsz)
             end
         fun jit_ldargp0 (jit_, v2, r0, v1, r1) =
            let val _  = jit_ldxi (jit_, v2, v1, 0)       (* v2 = Field(v1,0) *)
                val _  = jit_ldargl (jit_, r0, v1, 1)     (* r0 = Long_val(Field(v1,1)) *)
            in ()
            end
         fun jit_ldargp (jit_, v2, r0, v0, v1, r1, n) =
            let val _  = jit_ldxi (jit_, v1, v0, wsz * n) (* v1 = Field(v0,n) *)
                val _  = jit_ldargp0 (jit_, v2, r0, v1, r1)
            in ()
            end
      end
   end
end
