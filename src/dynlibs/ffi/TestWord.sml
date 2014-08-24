signature TestWordParams =
sig
   structure WordA : Word
   structure WordB : Word
end

signature TestWord =
sig
   val runTests : unit -> unit
   val timeA : unit -> int
   val timeB : unit -> int
end

functor TestWord(Params : TestWordParams) :> TestWord =
struct
  local
     structure WordA = Params.WordA
     structure WordB = Params.WordB
     structure WordAFormat =
        WordFormat (structure Word = Params.WordA)
          :> WordFormat where type word = Params.WordA.word
     structure WordBFormat =
        WordFormat (structure Word = Params.WordB)
          :> WordFormat where type word = Params.WordB.word
     structure CompTestWord =
       CompTestWord(structure WordA = WordA
                    structure WordB = WordB)
          :> CompTestWord where type wordA = Params.WordA.word
                            and type wordB = Params.WordB.word
     fun testopspeed which args =
         let val optst = 
               if which 
                  then fn (_,opA,_) => fn (wsA,_) => (SOME (opA wsA),NONE)
                  else fn (_,_,opB) => fn (_,wsB) => (NONE,SOME (opB wsB)) 
        in fn ops =>
                ignore (List.map (optst ops) args)
        end
     fun opspeed which = fn args => List.app (testopspeed which args)
     fun testop compare fmtA fmtB fmtARes fmtBRes args (opname,opA,opB) =
        let val res =
                 fn (wsA,wsB,rA,rB) => opname^" (A=("^(fmtA wsA)^
                                             "), B=("^(fmtB wsB)^
                                       ")) gave rA="^(fmtARes rA)^
                                              " rB="^(fmtBRes rB)
        in List.app
          (fn (wsA,wsB) => 
              let val rA = opA wsA
                  val rB = opB wsB
              in if compare (rA, rB)
                    then (if Globals.debug then print ((res(wsA,wsB,rA,rB))^"\n") else ())
                    else raise Fail  ("FAIL: "^(res(wsA,wsB,rA,rB)))
              end) args;
          if Globals.quiet
             then print "."
             else print ("OK: "^opname^" on "^(Int.toString (List.length args))^" arguments\n")
         end
     fun fmtpair toString = (fn (x,y) => "("^(toString x)^","^(toString y)^")")
     fun testbinops ops pairs = List.app (testop CompTestWord.wordsequal 
                                             (fmtpair WordAFormat.toString) (fmtpair WordBFormat.toString)
                                              WordAFormat.toString WordBFormat.toString pairs) ops
     fun testrelops ops pairs = List.app (testop (op =) 
                                             (fmtpair WordAFormat.toString) (fmtpair WordBFormat.toString)
                                                 Bool.toString Bool.toString pairs) ops
     fun testunops ops words = 
           List.app (testop CompTestWord.wordsequal
                     WordAFormat.toString WordBFormat.toString
                     WordAFormat.toString WordBFormat.toString words) ops
     
     fun testdbz () =
        let fun expectdiv opname thunk = 
                     (thunk ();"FAIL: "^opname^" didn't raise Div")
                      handle Div => ("OK: "^opname^" raises Div")
            fun dbyz binop = fn () => binop(WordA.fromInt 1,WordA.fromInt 0)
            val resdiv = expectdiv "div" (dbyz WordA.div)
            val resmod = expectdiv "mod" (dbyz WordA.mod)
        in if Globals.quiet
              then print ".."
              else print (resdiv^"\n"^resmod^"\n")
        end
     val testwords = CompTestWord.randwords Globals.ntests
     val testpairs = CompTestWord.mktestpairs testwords
     val testspeedwords = CompTestWord.randwords Globals.ntests
     val testspeedpairs = CompTestWord.mktestpairs testspeedwords
     fun test () =
        let val _ = testbinops CompTestWord.binops testpairs
            val _ = testrelops CompTestWord.relops testpairs
            val _ = testunops CompTestWord.unops testwords
            val _ = testdbz ()
        in ()
        end
     fun speedThunk which = 
         let val ospb = opspeed which testspeedpairs
             val ospr = opspeed which testspeedpairs 
             val ospu = opspeed which testspeedwords
         in  (fn () => (ospb CompTestWord.binops; 
                        ospr CompTestWord.relops;
                        ospu CompTestWord.unops))
         end
     val wordAspeed = speedThunk true 
     val wordBspeed = speedThunk false
  in
     val runTests = test
     val timeA = fn () => SpeedTest.timethis wordAspeed
     val timeB = fn () => SpeedTest.timethis wordBspeed
  end
end
