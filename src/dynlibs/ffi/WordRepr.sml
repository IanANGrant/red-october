structure WordRepr =
struct
   local fun bits w =
             let fun iter n 0w0 = n
                   | iter n w = iter (n+1) (Word.>>(w,0w1))
             in iter 0 w
             end
      val set : Word.word * int -> Word8Vector.vector = 
          fn (w,length) => 
            let val l = (length + 7) div 8
                val indexfn = if true
                                 then (fn i => i)
                                 else (fn i => l - i - 1)
                fun tabulatefn w =
                    fn i => let val b = indexfn i
                            in Word8.fromLarge (Word.andb((Word.>>(w,Word.*(Word.fromInt b,0w8))),0wxff))
                            end
            in if bits w > length 
                  then raise Size
                  else Word8Vector.tabulate (l,tabulatefn w)
            end
          val get : Word8Vector.vector -> Word.word = fn v =>
            let val nbits = 8 * (Word8Vector.length v)
                val fold = if true then Word8Vector.foldr else Word8Vector.foldl
                val foldfn = (fn (w,a) => Word.orb(Word.<<(a,0w8), Word8.toLarge w))
            in fold foldfn 0w0 v
            end
   in
      val word32Vec =
           fn w => set (w,32)
      val word16Vec =
           fn w => set (w,16)
      val word8Vec =
           fn w =>
             if w > 0wxFF
               then raise Size
               else Word8Vector.tabulate(1,fn _ => Word8.fromLarge w)
      val vecWord32 =
           fn v =>
             if Word8Vector.length v = 4
                then get v
                else raise Size
      val vecWord16 =
           fn v =>
             if Word8Vector.length v = 2
                then get v
                else raise Size
      val vecWord8 =
           fn v =>
             if Word8Vector.length v = 1
                then Word8.toLarge (Word8Vector.sub(v,0))
                else raise Size
   end
end
