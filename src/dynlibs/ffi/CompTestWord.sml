signature CompTestWord =
sig
   type wordA
   type wordB
   val binops : (string * (wordA * wordA -> wordA) * (wordB * wordB -> wordB)) list
   val mktestpairs : ('a * 'b) list -> (('a * 'a) * ('b * 'b)) list
   val randwords : int -> (wordA * wordB) list
   val relops : (string * (wordA * wordA -> bool) * (wordB * wordB -> bool)) list
   val unops : (string * (wordA -> wordA) * (wordB -> wordB)) list
   val wordsequal : wordA * wordB -> bool
end

functor CompTestWord (structure WordA : Word
                      structure WordB : Word)
  :> CompTestWord
        where type wordA = WordA.word
          and type wordB = WordB.word =
struct
  type wordA = WordA.word
  type wordB = WordB.word
  local
     structure WordAFormat = WordFormat (structure Word = WordA);
     structure WordBFormat = WordFormat (structure Word = WordB);
     val wordSize = Int.min(WordA.wordSize,WordB.wordSize)
     val topAbit = WordA.<<(WordA.fromInt 1,WordA.fromInt (wordSize - 1))
     val topBbit = WordB.<<(WordB.fromInt 1,WordB.fromInt (wordSize - 1))
     val maskAbits = WordA.andb(topAbit,WordA.-(topAbit,WordA.fromInt 1))
     val maskBbits = WordB.andb(topBbit,WordB.-(topBbit,WordB.fromInt 1))
     fun maskA w = WordA.andb(maskAbits,w)
     fun maskB w = WordB.andb(maskBbits,w)
     val fromAtoB = Option.valOf o WordB.fromString o WordAFormat.toString
     val fromBtoA = Option.valOf o WordA.fromString o WordBFormat.toString
     val IA = fromBtoA o fromAtoB
     val IB = fromAtoB o fromBtoA
     fun Iscan radix fmt scan = valOf o (StringCvt.scanString (scan radix)) o fmt radix
     fun Iscan2 radix = (Iscan radix WordB.fmt WordA.scan) o (Iscan radix WordA.fmt WordB.scan)
     fun Iscan2' radix = (Iscan radix WordA.fmt WordB.scan) o (Iscan radix WordB.fmt WordA.scan)
     fun Ipair radix = (Iscan2 radix,Iscan2' radix)
     val Ibinscan = Ipair StringCvt.BIN
     val Ioctscan = Ipair StringCvt.OCT
     val Idecscan = Ipair StringCvt.DEC
     val Ihexscan = Ipair StringCvt.HEX
     val op :*: = fn ((f,f'),(g,g')) => ((f o g),(f' o g'))
     infix 7 :*:
     val (scanA,scanB) = Ibinscan :*: Ioctscan :*: Idecscan :*: Ihexscan
     val gen = Random.newgen()
     fun randwordpair () =
           List.foldr (fn (b,(wA,wB)) => (WordA.+(WordA.<<(wA,WordA.fromInt 1),WordA.fromInt b),
                                          WordB.+(WordB.<<(wB,WordB.fromInt 1),WordB.fromInt b)))
                      (WordA.fromInt 0,WordB.fromInt 0)
                      (Random.rangelist (0,2) (wordSize,gen))
    fun handleDiv def oper = (fn args => oper args handle Div => def) 
  in
     val randwords =
           let fun iter r 0 = r
                 | iter r n = iter ((randwordpair())::r) (n-1)
           in iter []
           end
     fun mktestpairs pl =
            List.concat 
              (List.map 
                 (fn (xA,xB) =>
                      List.map 
                         (fn (yA,yB) => ((xA,yA),(xB,yB)))
                          pl)
               pl)
     val binops = [("andb", WordA.andb,      WordB.andb),
                   ("orb",  WordA.orb,       WordB.orb),
                   ("xorb", WordA.xorb,      WordB.xorb),
                   ("+",    maskA o WordA.+, maskB o WordB.+),
                   ("-",    maskA o WordA.-, maskB o WordB.-),
                   ("*",    maskA o WordA.*, maskB o WordB.*),
                   ("div",  handleDiv (maskA (WordA.fromInt 42)) WordA.div,
                            handleDiv (maskB (WordB.fromInt 42)) WordB.div),
                   ("mod",  handleDiv (maskA (WordA.fromInt 42)) WordA.mod,
                            handleDiv (maskB (WordB.fromInt 42)) WordB.mod)]
     val relops = [("<" , WordA.<,  WordB.<),
                   ("<=", WordA.<=, WordB.<=),
                   (">" , WordA.>,  WordB.>),
                   (">=", WordA.>=, WordB.>=)]
     val unops = [("not",      maskA o WordA.notb, maskB o WordB.notb),
                  ("fmt/scan", scanA,              scanB),
                  ("~",        maskA o WordA.~,    maskB o WordB.~)]
     fun wordsequal (wA, wB) = WordB.compare (fromAtoB wA,wB) = EQUAL
                       andalso WordA.compare (wA,fromBtoA wB) = EQUAL
   end
end
