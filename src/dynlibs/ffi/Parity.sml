
local
   fun popcnt n =
      let val b = Word8.fromInt n
          fun iter r 0w0 = Word8.toInt r 
            | iter r p =
                 let val p = p - 0w1
                     val m = Word8.<<(0wx1,p)
                 in iter (r + (Word8.>>(Word8.andb(b,m),p))) p
                 end
      in iter 0w0 0w8
      end
   fun parity_ n = 
      let val pop = Word8.fromInt (popcnt n)
      in Word8.andb(pop,0w1)
      end
   val lu = Word8Vector.tabulate (255,parity_)
in
   fun parity w = Word8Vector.sub(lu, Word8.toInt w)
end

fun mkEntropyPool size =
   let val pool = mkRingBuffer size
       val byte = ref 0w0
       val bits = ref 0w0
       val buf = mkRingBuffer (8 * size)
       val seed = Time.toReal (Time.now ())
       val gen = Random.newgenseed seed
       fun newoffs () = Word8.fromInt (Random.range (0,256) gen)
       val offs = ref 0w0
       val used = ref 0
       val renew = 15 (* Change the offset at most every RENEW bytes *)
       fun renewoffs () = (used := 0;
                           offs := (newoffs()))
       fun initoffs () = (* This is to "burn in" the PRN generator,
                            because two of the Moscow ML PRN generators 
                            for eample, if started with seeds that do not differ
                            greatly in magnitude seem to almost always
                            produce the same first 8 bits. *)
           let fun iter 0 = renewoffs ()
                 | iter n = (ignore (newoffs ()); iter (n-1))
           in iter (Word8.toInt (newoffs()))
           end
       fun mayberenewoffs n =
              (used := (!used) + n;
               if !used > renew
                  then renewoffs()
                  else ())
       fun need () = ((#free pool) () - 1) * 8 + (8 - (Word.toInt(!bits)))
       fun avail () = (#avail pool) ()
       fun addbit b = (if b = 0w1
                          then byte := Word8.orb (!byte,Word8.<<(0wx1,(!bits)))
                          else ();
                       bits := ((!bits) + 0w1);
                       if (!bits) = 0wx8
                          then if (#free pool)() >= 1
                               then ((#writeByte pool) (!byte);
                                     bits := 0w0;
                                     byte := 0w0)
                               else ()
                          else ())
       fun addByte w =  (* We cycle the incoming bytes using an offset
                           from the PRNG so that even a very low entropy
                           sequence still generates some randomness. This
                           is just disaster mitigation. The source should
                           always be high entropy. *)
           (offs := Word8.mod(Word8.+(w,!offs),0wxFF);
            addbit (parity (!offs)))
       fun addString s =
           let val n = Int.min (need(), (String.size s))
               val n' = if n < 0 then 0 else n
               val ss = Substring.substring (s,0,n)
           in Substring.app (addByte o Word8.fromInt o Char.ord) ss;
              mayberenewoffs n'
           end
       fun addVec vec =
           let val n = Int.min (need(), (Word8Vector.length vec))
               val n' = if n < 0 then 0 else n
               val slc = Word8VectorSlice.slice (vec,0,SOME n)
           in Word8VectorSlice.app addByte slc
           end
       fun getByte () =
             if avail() = 0
                then raise Fail "Not enough entropy"
                else (#readByte pool)()
   in initoffs();
      {    need = need,
          avail = avail, 
        addByte = addByte, 
      addString = addString,
         addVec = addVec,
        getByte = getByte }
   end
