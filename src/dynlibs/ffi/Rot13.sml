functor Rot13(type X
              val a:X
              val A:X
              val isLower:X->bool
              val isUpper:X->bool
              val ord:X->int
              val chr:int->X) =
struct
   fun encode i = 
      let val c = chr i
          fun base c = ord (if isLower c 
                               then a
                               else if isUpper c
                                       then A
                                       else c)
      in if isUpper c orelse isLower c
            then let val b = base c
                 in b + (i - b + 13) mod 26
                 end
            else i
      end
   val decode = encode
end

structure Ebg13Jbeq8 =
struct
   local
      structure Rot13struct=
         Rot13(type X=Char.char
               val a = #"a"
               val A = #"A"
               val isUpper=Char.isUpper
               val isLower=Char.isLower
               val ord=Char.ord
               val chr=Char.chr)
      val r13 = Word8Vector.tabulate (256, Word8.fromInt o Rot13struct.encode)
   in
      fun encode w = Word8Vector.sub (r13, Word8.toInt w)
      val qrpbqr = encode
   end
end
