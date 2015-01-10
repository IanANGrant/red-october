local
    open Dynlib

    val dlxh = Dynlib.dlopen {lib = "",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }

    val selfsym = Dynlib.dlsym dlxh

    val selfapp1 : string -> 'a -> 'b
         = fn s => Dynlib.app1 (selfsym s)
in
   fun doubleSlc x =
      let val doubleVec_ : real -> Word8Vector.vector = selfapp1 "doubletow8vec"
          val v = doubleVec_ x
          val a = Word8Array.array (8,0w0)
      in Word8Array.copyVec {di = 0, dst = a, src = v};
         Word8ArraySlice.full a
      end

   fun slcDouble (s : Word8ArraySlice.slice) =
      let val vecDouble_ : Word8Vector.vector -> real = selfapp1 "w8vectodouble"
      in if Word8ArraySlice.length s = 8
            then vecDouble_ (Word8ArraySlice.vector s)
         else
            raise Fail "slcDouble: wrong argument length"
      end

   fun floatSlc x =
      let val floatVec_ : real -> Word8Vector.vector = selfapp1 "floattow8vec"
          val v = floatVec_ x
          val a = Word8Array.array (8,0w0)
      in Word8Array.copyVec {di = 0, dst = a, src = v};
         Word8ArraySlice.full a
      end

   fun slcFloat (s : Word8ArraySlice.slice) =
      let val vecFloat_ : Word8Vector.vector -> real = selfapp1 "w8vectofloat"
      in if Word8ArraySlice.length s = 4
            then vecFloat_ (Word8ArraySlice.vector s)
         else
            raise Fail "slcFloat: wrong argument length"
      end

   val floatVec : real -> Word8Vector.vector = selfapp1 "floattow8vec"

   fun vecFloat (v : Word8Vector.vector) =
      let val vecFloat_ : Word8Vector.vector -> real = selfapp1 "w8vectofloat"
      in if Word8Vector.length v = 4
            then vecFloat_ v
            else raise Fail "vecFloat: wrong argument length"
      end

   val doubleVec : real -> Word8Vector.vector = selfapp1 "doubletow8vec"

   fun vecDouble (v : Word8Vector.vector) =
       let val vecDouble_ : Word8Vector.vector -> real = selfapp1 "w8vectodouble"
       in if Word8Vector.length v = 8
             then vecDouble_ v
             else raise Fail "vecDouble: wrong argument length"
       end
end
