functor ArraySliceWordEnc(structure PrimEnc : PrimEnc
                          structure Word : WordPrim)
    :> WordEnc where type excn = PrimEnc.excn =
struct
   local
       prim_eqtype array_
       prim_val array_   : int -> array_                       = 1 "create_string"
       prim_val fill_    : array_ -> int -> int -> int -> unit = 4 "fill_string"
       type array = array_ ref
       open Jit
   in
      open PrimEnc
      type word = (array * int * int)
      type largeword = Word.word 
      val wordSize = WORDSIZE
      local
         val wsz = WORDSIZE div 8
         prim_val fromWord8ArraySlice_ : Word8ArraySlice.slice -> word = 1 "identity"
         prim_val toWord8ArraySlice_ : word -> Word8ArraySlice.slice = 1 "identity"
      in
         val fromWord8ArraySlice = fromWord8ArraySlice_ 
         val toWord8ArraySlice = toWord8ArraySlice_ 
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
             let val a = array_ wsz 
             in fill_ a 0 wsz 0;
                (ref a, 0, wsz)
             end
         fun jit_ldargp0 (jit_, v2, r0, v1, r1) =
            let val _  = jit_ldxi (jit_, r1, v1, 0)       (* r1 = Field(v1,0) *)
                val _  = jit_ldxi (jit_, v2, r1, 0)       (* v2 = Field(r1,0) *)
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
