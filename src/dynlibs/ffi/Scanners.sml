local
   fun scanner match_elt f =
          fn sus => 
             case match_elt sus
               of SOME (v,rest) =>
                    (case f v
                       of SOME v => SOME (v,rest)
                        | NONE => NONE)
                | NONE => NONE
in
   val atomicScanner = scanner
end

local
   fun scanner match_delim match_elt f g acc =
       let fun iter acc =
          fn sus =>
              case match_elt sus
                of SOME (v,rest) => 
                     (case (f (v,acc))
                        of (SOME acc) => 
                              (case match_delim rest
                                 of (SOME (_,rest)) => iter acc rest
                                  | NONE => g (acc,rest))
                         | NONE => NONE)
                 | NONE => NONE
       in iter acc
       end
in
   fun listScanner delim elt pref nilv cons postf =
      let fun f (v,acc) =
                 case pref v
                   of SOME x => SOME (cons (x,acc))
                    | NONE => NONE
          fun g (acc,rest) =
                 case postf acc
                   of SOME x => SOME (x,rest)
                    | NONE => NONE
      in scanner delim elt f g nilv
      end
end

local
   fun scanner match_elt1 match_elt2 f1 f2 g =
          fn sus => 
             case match_elt1 sus
               of SOME (v,rest) => 
                    (case f1 v
                       of SOME v1 => 
                            (case match_elt2 rest
                               of (SOME (v,rest)) =>
                                     (case f2 v
                                        of SOME v2 =>
                                             (case g(v1,v2)
                                                of SOME v => SOME (v,rest)
                                                 | NONE => NONE)
                                         | NONE => NONE)
                                | NONE => NONE)
                        | NONE => NONE)
                | NONE => NONE
in
   fun seqScanner match_elt1 match_elt2 f1 f2 cons postf = 
      let fun g (x1,x2) = postf (cons (x1,x2))
      in scanner match_elt1 match_elt2 f1 f2 g 
      end
end

local
   fun scanner match_elt1 match_elt2 f1 f2 g =
          fn sus => 
             case match_elt1 sus
               of SOME (v,rest) => 
                    (case f1 v
                       of SOME v1 => (case g v1 of SOME v => SOME (v,rest) | NONE => NONE)
                       | NONE => NONE)
                | NONE =>
                    (case match_elt2 sus
                       of (SOME (v,rest)) =>
                             (case f2 v
                                of SOME v2 => (case g v2 of SOME v => SOME (v,rest) | NONE => NONE)
                                | NONE => NONE)
                        | NONE => NONE)
in
   val altScanner = scanner
end

local
   fun scanner match_elt f =
          fn sus => 
             case match_elt sus
               of SOME (v,rest) =>
                     SOME (f v,rest)
                | NONE => SOME (NONE,sus)
in
   val optScanner = scanner
end
