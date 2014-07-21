load "IntInf";

local open IntInf
   val num = fromInt (Int.~ (Char.ord #"*"))
   val (slc,nwords,sgn) = export rawformat num
   val sll' = Word8ArraySlice.foldr (fn (w,a) => w::a) [] slc
   val num' = import rawformat (slc,nwords,Int.~ sgn)
   val num'n = toInt num'
   val numn = toInt num
   val m = init2 (20,0)
   val binstr = fmt StringCvt.BIN
   val octstr = fmt StringCvt.OCT
   val hexstr = fmt StringCvt.HEX
   val decstr = fmt StringCvt.DEC
in
   val theslice = sll'
   val 42 = num'n
   val "~42" = decstr num
   val () = setbit (m,0)
   val 1 = toInt m
   val (true,false) = (tstbit(fromInt 0xff, 7),
                       tstbit(fromInt 0xff, 8))
   val () = setbit (m, 7)
   val "10000001" = binstr m
   val () = combit (m,7)
   val 1 = toInt m
   val () = combit (m, 6)
   val 0x41 = toInt m
   val () = clrbit (m, 6)
   val 1 = toInt m
   val NONE = popcount (fromInt ~0xff)
   val NONE = hamdist (fromInt ~0xff, fromInt 1)
   val 8 = toInt(valOf (popcount (fromInt 0xff)))
   val 5 = toInt(valOf (hamdist (fromInt 0xff, fromInt 7)))
   val 1 = toInt(valOf (popcount ((fromInt 0x10000) * (fromInt 0x10000))))
   val "40" = octstr(valOf (popcount ((fromInt 0x10000) * (fromInt 0x10000) - fromInt 1)))
   val 7 = msb(fromInt 0xff)
   val 5 = toInt(sqrt(fromInt 25))
   val (5,2) =
         let val (q,r) = (sqrtrem(fromInt 27))
         in (toInt q, toInt r)
         end
   val 7 = toInt(valOf(scan1(fromInt 128, 0)))
   val 8 = toInt(valOf(scan0(fromInt 0xff, 0)))
   val NONE = scan0(fromInt ~1,0)
   val "400" = octstr (op << (fromInt 1,8))
   val 1 = toInt(op >> (fromInt 256,8))
   val ~1 = toInt (op ~>> (fromInt ~1,2))
   val 1 = toInt ((fromInt ~1) mod (fromInt 2))
   val 7 = toInt ((fromInt ~1) mod (fromInt 8))
   val "~200" = octstr (op ~>> (fromInt ~256,1))
   val "~128" = decstr (op >> (fromInt ~256,1))
   val "~80" = hexstr (op >> (fromInt ~0x100,1))
   val 0xff = toInt (notb (~ (op << (fromInt 1,8))))
end
