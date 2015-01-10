val _ = List.app Meta.load
          ["Real", "Regex", "Substring", "Scanners", "Printers"];

val match_int = Int.scan StringCvt.DEC Substring.getc
val match_word = Word.scan StringCvt.HEX Substring.getc
val match_real = Real.scan Substring.getc

val preproc = fn p => fn x => if p x then SOME x else NONE

val p = fn n => n >= 0 andalso n <= 255

val Int8Scanner = Scanners.atomicScanner match_int (preproc p)
val WordScanner = Scanners.atomicScanner match_word (fn w => SOME w)
val IntScanner = Scanners.atomicScanner match_int (fn i => SOME i)
val RealScanner = Scanners.atomicScanner match_real (fn r => SOME r)

(*--------------------------------------------------------------------------------

   Some Real Greek Lego from 300BC Alexandria, courtesy of Diophantus.

+ val getOpt         : 'a option * 'a -> 'a 
+ val isSome         : 'a option -> bool 
+ val valOf          : 'a option -> 'a 
+ val filter         : ('a -> bool) -> 'a -> 'a option 
+ val map            : ('a -> 'b) -> 'a option -> 'b option
+ val app            : ('a -> unit) -> 'a option -> unit
+ val join           : 'a option option -> 'a option
+ val compose        : ('a -> 'b) * ('c -> 'a option) -> ('c -> 'b option)
+ val mapPartial     : ('a -> 'b option) -> ('a option -> 'b option)
+ val composePartial : ('a -> 'b option) * ('c -> 'a option) -> ('c -> 'b option)

----------------------------------------------------------------------------------*)

val anchor = "^"
val optws = "[ \\t]*"

fun re_lit s =
   let val getc = Substring.getc
       fun loop (NONE,acc) = acc
         | loop (SOME (c,rest),acc) = 
             case c
               of #"." => loop (getc rest,acc^"\\"^".")
                | #"[" => loop (getc rest,acc^"\\"^"[")
                | #"*" => loop (getc rest,acc^"\\"^"*")
                | _ => loop (getc rest,acc^(String.str c))
   in loop (getc (Substring.full s),"")
   end

fun delim rexp =
      Regex.getItem (Regex.regcomp (anchor^optws^rexp) []) []

fun litDelim s =
   delim (re_lit s)

fun rescanner rexp =
   let val scan = Regex.getItem (Regex.regcomp (rexp) []) []
   in fn ss => 
      case scan ss
        of SOME (ssv,r) => SOME (Substring.string (Vector.sub (ssv,0)),r)
         | _ => NONE
   end

val I = fn b => b
val fInt = Int.toString
val fStr = I : string -> string

val fReal = Real.toString
val fWord = Word.toString

(* mosml won't accept this as a type for dec.  I don't see anything
   wrong with it though: (dec : ('a -> 'b -> 'c) -> 'c -> 'd -> 'c) *)

fun pList_ dec =
  fn pd =>
   fn pe =>
    fn c =>
      fn (lst,acc) =>
        let fun decCons h t = SOME (h,t)
            val decNil = NONE
            val decList = dec decCons decNil
            fun loop _ (NONE,acc) = acc
              | loop first (SOME(h,t),acc) = 
                  let val acc = if first then acc else pd c acc
                  in loop false (decList t, pe c (h,acc))
                  end
        in loop true (decList lst, acc)
        end

fun pPair_ dec =
  fn pl => fn pm => fn pr =>
    fn (px,py) =>
     fn c =>
      fn (v,acc) =>
        let fun decPair x y = (x,y)
            val (x,y) = dec decPair v
            val acc = pl c acc
            val acc = px c (x,acc)
            val acc = pm c acc
            val acc = py c (y,acc)
            val acc = pr c acc
        in acc
        end

fun pSeq_ dec =
    fn (p1,p2) =>
     fn c =>
      fn (v,acc) =>
        let fun decSeq x y = 
                let val acc = p1 c (x,acc)
                    val acc = p2 c (y,acc)
                in acc end
        in dec decSeq v
        end

fun pAlt_ dec pl pr c =
   let fun printer (l,acc) =
          let val elt1p = fn v => pl c (v,acc)
              val elt2p = fn v => pr c (v,acc)
              val acc' = dec elt1p elt2p l
          in acc'
          end
   in printer
   end

fun pfStr c = fn (s,acc) => c (fStr s,acc)
fun pfReal c = fn (r,acc) => c (fReal r,acc)
fun pfInt c = fn (i,acc) => c (fInt i,acc)
fun pfWord c = fn (w,acc) => c (fWord w,acc)
fun pfLit s = fn c => fn acc => pfStr c (s,acc)
fun pfConst s = fn c => fn (_,acc) => pfStr c (s,acc)

val sfReal = (pfReal, RealScanner)
val sfWord = (pfWord, WordScanner)
val sfInt =  (pfInt,  IntScanner)
fun sfStr re = (pfStr, rescanner re)
fun sfLit s = (pfConst s, litDelim s)

datatype ('a,'b) inj =
   LInj of 'a
 | RInj of 'b

fun sList_ vnil fcons preproc postproc delim =
    fn se =>
       Scanners.listScanner (* XXX : change Scanners.* to match this order *)
          delim se preproc vnil fcons postproc

fun sSeq_ fcons f1 f2 postproc =
   fn (sl,sr) => Scanners.seqScanner sl sr f1 f2 fcons postproc;

fun sAlt_ f1 f2 g =
   fn (sl,sr) => Scanners.altScanner sl sr f1 f2 g;

fun sOpt_ f =
   fn se => Scanners.optScanner se f;

fun sParen_ (sl,sv,sr) =
  let val prep = fn x => SOME x
  in Scanners.seqScanner
          (Scanners.seqScanner
             sl
             sv
             prep prep (fn (_,x) => x) prep)
          sr
          prep prep (fn (x,_) => x) prep
   end

fun sPair_ (sl,sm,sr) =
  let val prep = fn x => SOME x
  in Scanners.seqScanner
          (Scanners.seqScanner
             sl
             sm
             prep prep (fn (x,_) => x) prep)
          sr
          prep prep (fn (x,y) => (x,y)) prep
   end

(* The local binding keeps these functions let-polymorphic,
   even with mosml's valuepoly := true *)

local
   fun sfList delim prep postp =
      let fun dec fcons vnil =
                 fn [] => vnil
                  | (h::t) => fcons h t
          val fcons = op ::
          val vnil = [] 
          fun preproc p n = if p n then SOME n else NONE
          fun postproc p l =
                if p l
                   then SOME (List.rev l)
                   else NONE
         in (fn pe =>
             fn c =>
                pList_ dec (pfLit delim) pe c,
          fn se => 
             fn c =>
                sList_ vnil fcons (preproc prep) (postproc postp) (litDelim delim) se c)
      end
   fun sfPair_ decon recon l m r =
      let fun dec p = (fn f => fn (x,y) => (f x y)) p
      in fn ((pl,sl),(pr,sr)) =>
          (fn c => (pPair_ dec (pfLit l) (pfLit m) (pfLit r) (pl,pr) c) o (fn (a,b) => (decon a,b)),
           fn c => sParen_ (litDelim l, 
                               Option.compose
                                  (fn (a,z) => (recon a,z),
                                          (sPair_ (sl,litDelim m,sr))), litDelim r) c)
      end
   fun sfPair l m r =
      let fun dec p = (fn f => fn (x,y) => f x y) p
      in fn ((pl,sl),(pr,sr)) =>
          (fn c =>
                pPair_ dec (pfLit l) (pfLit m) (pfLit r) (pl,pr) c,
           fn c => sParen_ (litDelim l, sPair_ (sl,litDelim m,sr), litDelim r) c)
      end
   fun sfSeq f1 f2 preproc postproc =
        fn ((pl,sl),(pr,sr)) =>
            let fun dec p = (fn f => fn (x,y) => f x y) p
            in (fn c => fn (v,s) => pSeq_ dec (pl,pr) c (preproc v,s),
                fn c => sSeq_ (fn x => x) f1 f2 postproc (sl,sr) c)
            end
   fun sfAlt (pl,sl) (pr,sr) =
        let val dec = (fn f1 => fn f2 => fn (LInj a) => f1 a
                                          | (RInj a) => f2 a)
            fun f1 l = SOME (LInj l)
            fun f2 r = SOME (RInj r)
            fun g l = SOME l
        in (fn c => pAlt_ dec pl pr c,
            fn c => sAlt_ f1 f2 g (sl,sr) c)
        end
   fun sfOpt (pl,sl) =
        let val dec = (fn f1 => fn f2 => fn (SOME a) => f1 a
                                          | (NONE) => f2 ())
            fun f l = SOME l
        in  (fn c => pAlt_ dec pl (fn c => fn (_,acc) => acc) c,
            fn c => sOpt_ f sl c)
        end
   fun sfList_ delim =
       fn (print,scan) =>
          let val prep = (fn _ => true)
              val postp = (fn _ => true)
              val (pr,sc) = sfList delim prep postp
          in (fn p => pr print p, fn c => sc scan c)
          end
   fun sfPairPlain p = sfPair "(" "," ")" p
   fun sfListPlain p = sfList_ "," p
   fun sfPairSq p = sfPair "[" "," "]" p
   fun sfPairBraKet p = sfPair "〈"  "|" "〉" p
   fun sfPairLR p = sfPair "[Left:" ", Right:" "]" p
   fun sfPairAlt p = sfPair "[" "|" "]" p
in (* Here are some examples. The eta-conversions are needed to keep
      the mad dog of value polymorphism at bay until the consumer is
      applied This allows us to iteratively compose the scanners and
      formatters. *)
   val sfIPv4Addr =
          let val prep = (fn _ => true)
              val postp = (fn l => List.length l = 4)
              val (pr,sc) = sfList "." prep postp
          in (fn p => pr pfInt p, fn c => sc Int8Scanner c)
          end
   val sfSeqPlain =
       fn args =>
          let val f1 = (fn x => SOME x)
              val f2 = (fn y => SOME y)
              val postproc = fn p => SOME p
              val preproc = (fn x => x)
          in sfSeq f1 f2 preproc postproc args
          end
   fun sfSeqComp conv iconv =
       fn args =>
          let val f1 = (fn x => SOME x)
              val f2 = (fn y => SOME y)
              val postproc = fn p => SOME (conv p)
              val preproc = (fn t => iconv t)
          in sfSeq f1 f2 preproc postproc args
          end
   val conv3 = fn (x,(y,z)) => (x,y,z)
   val iconv3 = fn (x,y,z) => (x,(y,z))
   val conv4 = fn (w,(x,y,z)) => (w,x,y,z)
   val iconv4 = fn (w,x,y,z) => (w,(x,y,z))
   val conv5 = fn (v,(w,x,y,z)) => (v,w,x,y,z)
   val iconv5 = fn (v,w,x,y,z) => (v,(w,x,y,z))
   val conv6 = fn (u,(v,w,x,y,z)) => (u,v,w,x,y,z)
   val iconv6 = fn (u,v,w,x,y,z) => (u,(v,w,x,y,z))
   val conv7 = fn (t,(u,v,w,x,y,z)) => (t,u,v,w,x,y,z)
   val iconv7 = fn (t,u,v,w,x,y,z) => (t,(u,v,w,x,y,z))
   val conv8 = fn (s,(t,u,v,w,x,y,z)) => (s,t,u,v,w,x,y,z)
   val iconv8 = fn (s,t,u,v,w,x,y,z) => (s,(t,u,v,w,x,y,z))
   val conv9 = fn (r,(s,t,u,v,w,x,y,z)) => (r,s,t,u,v,w,x,y,z)
   val iconv9 = fn (r,s,t,u,v,w,x,y,z) => (r,(s,t,u,v,w,x,y,z))
   val conva = fn (q,(r,s,t,u,v,w,x,y,z)) => (q,r,s,t,u,v,w,x,y,z)
   val iconva = fn (q,r,s,t,u,v,w,x,y,z) => (q,(r,s,t,u,v,w,x,y,z))
   val convb = fn (p,(q,r,s,t,u,v,w,x,y,z)) => (p,q,r,s,t,u,v,w,x,y,z)
   val iconvb = fn (p,q,r,s,t,u,v,w,x,y,z) => (p,(q,r,s,t,u,v,w,x,y,z))
   val convc = fn (n,(p,q,r,s,t,u,v,w,x,y,z)) => (n,p,q,r,s,t,u,v,w,x,y,z)
   val iconvc = fn (n,p,q,r,s,t,u,v,w,x,y,z) => (n,(p,q,r,s,t,u,v,w,x,y,z))
   fun sfSeqDelimVal d =
       fn args =>
          let val f1 = (fn x => SOME x)
              val f2 = (fn y => SOME y)
              val postproc = fn (_,v) => SOME v
              val preproc = (fn x => ((),x))
          in sfSeq f1 f2 preproc postproc (sfLit d, args)
          end
   fun sfSeqTwo d (a,b) = sfSeqPlain (a, sfSeqDelimVal d b)
   fun sfSeqThree d (a,b,c) =
         sfSeqComp conv3 iconv3 (a, sfSeqDelimVal d (sfSeqTwo d (b,c)))
   fun sfSeqFour d' (a,b,c,d) =
         sfSeqComp conv4 iconv4 (a, sfSeqDelimVal d' (sfSeqThree d' (b,c,d))) 
   fun sfSeqFive d' (a,b,c,d,e) =
         sfSeqComp conv5 iconv5 (a, sfSeqDelimVal d' (sfSeqFour d' (b,c,d,e)))
   fun sfSeqSix d' (a,b,c,d,e,f) =
         sfSeqComp conv6 iconv6 (a, sfSeqDelimVal d' (sfSeqFive d' (b,c,d,e,f)))
   fun sfSeqSeven d' (a,b,c,d,e,f,g) =
         sfSeqComp conv7 iconv7 (a, sfSeqDelimVal d' (sfSeqSix d' (b,c,d,e,f,g)))
   fun sfSeqEight d' (a,b,c,d,e,f,g,h) =
         sfSeqComp conv8 iconv8 (a, sfSeqDelimVal d' (sfSeqSeven d' (b,c,d,e,f,g,h)))
   fun sfSeqNine d' (a,b,c,d,e,f,g,h,i) =
         sfSeqComp conv9 iconv9 (a, sfSeqDelimVal d' (sfSeqEight d' (b,c,d,e,f,g,h,i)))
   fun sfSeqTen d' (a,b,c,d,e,f,g,h,i,j) =
         sfSeqComp conva iconva (a, sfSeqDelimVal d' (sfSeqNine d' (b,c,d,e,f,g,h,i,j)))
   fun sfSeqEleven d' (a,b,c,d,e,f,g,h,i,j,k) =
         sfSeqComp convb iconvb (a, sfSeqDelimVal d' (sfSeqTen d' (b,c,d,e,f,g,h,i,j,k)))
   fun sfSeqTwelve d' (a,b,c,d,e,f,g,h,i,j,k,l) =
         sfSeqComp convc iconvc (a, sfSeqDelimVal d' (sfSeqEleven d' (b,c,d,e,f,g,h,i,j,k,l)))
   val sfAlt0 = sfAlt (sfStr "[A-Z]*") sfReal
   val sfOpt0 = sfOpt sfReal
   val sfIP = sfIPv4Addr
   val sfLst0 = sfListPlain sfIPv4Addr
   val sfLst1 = sfListPlain sfReal
   val sfLst2 = sfListPlain sfInt
   val sfLst3 = sfListPlain (sfStr "[A-Z]*")
   val sfSeq0 = sfSeqTwo "," (sfReal, sfInt)
   val sfSeq1 = sfSeqThree "," (sfWord, sfReal, sfInt)
   val sfSeq2 = sfSeqFour "," (sfStr (optws^"[A-Z]*"),sfWord, sfReal, sfInt)
   val sfSeq3 = sfSeqFive "," (sfInt,sfStr (optws^"[A-Z]*"),sfWord, sfReal, sfInt)
   val sfSeq4 = sfSeqTwelve "," (sfInt, sfStr (optws^"[A-Z]*"), sfWord, sfReal, sfInt,
                                 sfInt, sfStr (optws^"[A-Z]*"), sfWord, sfReal, sfInt,
                                 sfReal, sfInt)
   val sfPr0 = sfPairSq (sfReal,sfInt)
   val sfPr1 = sfPairBraKet (sfPr0,sfWord)
   val sfPr1' = sfPairPlain (sfReal,sfWord)
end

(* A simple consumer and an empty state for it. *)
val strLP = fn (v,s) => s^v
val strLNull = ""

(* A simple producer *)
fun prod scanner =
   fn s =>
      let val err = fn (s,ss) => Fail ("Invalid input "^s^" before "^ss)
      in case scanner (Substring.full s)
           of SOME (ip,ss) =>
                   if Substring.size ss = 0
                      then ip
                      else raise (err (s,Substring.string ss))
            | NONE => raise (err (s,"<EOS>"))
      end

fun optprod scanner =
   fn s =>
      let val err = fn (s,ss) => Fail ("Invalid input "^s^" before "^ss)
      in case scanner (Substring.full s)
           of SOME (ip,ss) => 
                 if Substring.size ss = 0
                    then ip
                   else raise (err (s,Substring.string ss))
            | NONE => NONE
      end

fun mkPrSc (pr,sc)    = (pr strLP, prod sc)
fun mkPrScOpt (pr,sc) = (pr strLP, optprod sc)

val (popt0,scanopt0)  = mkPrScOpt sfOpt0
val (palt0,scanalt0)  = mkPrSc sfAlt0
val (pppr0,scanpr0)   = mkPrSc sfPr0
val (pppr1,scanpr1)   = mkPrSc sfPr1
val (ppIPl0,scanIPl0) = mkPrSc sfLst0
val (ppIP0,scanIP0)   = mkPrSc sfIP
val (pps0,scans0)     = mkPrSc sfSeq0
val (pps1,scans1)     = mkPrSc sfSeq1
val (pps3,scans3)     = mkPrSc sfSeq3
val (pps4,scans4)     = mkPrSc sfSeq4
val (ppl1,scanl1)     = mkPrSc sfLst1
val (ppl2,scanl2)     = mkPrSc sfLst2
val (ppl3,scanl3)     = mkPrSc sfLst3

val pout = strLNull

val rs = [palt0 (LInj "AB",              pout),
          palt0 (RInj 42.0,              pout),
          palt0 (scanalt0 "42.0",        pout),
          palt0 (scanalt0 "CC",          pout),
          popt0 (scanopt0 "42.0",        pout),
          popt0 (scanopt0 "",            pout),
          ppIP0 (scanIP0 "192 .168 .0.1",pout),
          ppIPl0 (scanIPl0
                  "192 .168 .0.1,\
                 \ 192.168.1.1",         pout),
          ppl1 (scanl1 "42.0,43.0,42.0", pout),
          ppl2 (scanl2 "42,43,42",       pout),
          ppl3 (scanl3 "BA,AB,C",        pout),
          pps0 (scans0 "42.0,130",       pout),
          pps3 (scans3 "42,ABC,2a,42.0,130", pout),
          pps4 (scans4 "42, ABC, 2a, 42.0 , 130,\
                      \ 42,ABC,2a,42.0,130,\
                      \ 43.0,42",        pout),
          pppr0 (scanpr0 "[42.0,42]",    pout),
          pppr1 (scanpr1 "  〈 [ 42.0 ,\
                        \ 42 ] | 2B 〉",  pout)];
