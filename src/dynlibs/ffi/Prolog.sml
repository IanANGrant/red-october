(* This is a transliteration of the Prolog interpreter in John
   Harrison's Cambridge lecture notes. *)

datatype term =
   Fn of string * term list
 | Var of string

datatype outcome =
   Yes of (string * term) list
 | No

local open GrammarSyntax
   fun parseRulesPlain file stream lexbuf =
       let val expr = PrologParser.File PrologLexer.Token lexbuf
       in
          Parsing.clearParser();
   	  expr
       end handle exn => (Parsing.clearParser(); raise exn);
   fun parseTermPlain file stream lexbuf =
       let val expr = PrologParser.OneTerm PrologLexer.Token lexbuf
       in
          Parsing.clearParser();
          expr
       end handle exn => (Parsing.clearParser(); raise exn);
   fun createLexerString (s : string) =
      Lexing.createLexerString s
   fun processString pfn s =
      pfn "string" s (createLexerString s)
   val parse_rules_string = processString parseRulesPlain
   val parse_term_string = processString parseTermPlain
   fun qlToString l =
      let fun iter r [] = r
            | iter r ((QUOTE s)::fs) = iter (r^s) fs
            | iter r ((ANTIQUOTE s)::fs) = iter (r^s) fs
      in iter "" l
      end
   fun parserulesq rules =
      parse_rules_string (qlToString rules)
   fun parsetermq term =
      parse_term_string (qlToString term)
   fun fails s t = (printTree t;raise Fail (s^": no case."))
   fun mkTerm (NonTerm("atom",
                       [NonTerm("constant",
                                [Term(name)]),
                        l as NonTerm("term-list",_)]))
           = Fn(name,mkArgs l)
     | mkTerm (NonTerm("atom",[NonTerm("constant",[Term(name)])]))
           = Fn(name,[])
     | mkTerm (NonTerm("atom",[NonTerm("variable",[Term(name)])]))
           = Var(name)
     | mkTerm (NonTerm("atom",[l as NonTerm("list",_)]))
           = mkList(l)
     | mkTerm t = fails "mkTerm" t
   and mkList (NonTerm("list",[a as NonTerm("atom",_),
                               l as NonTerm("list",_)]))
           = Fn(".",[mkTerm a,mkList l])
     | mkList (NonTerm("list",[a as NonTerm("atom",_)]))
           = Fn(".",[mkTerm a,Fn("[]",[])])
     | mkList (NonTerm("list",[a as NonTerm("atom",_),a' as NonTerm("atom",_)]))
           = Fn(".",[mkTerm a,mkTerm a'])
     | mkList (NonTerm("list",[]))
           = Fn("[]",[])
     | mkList t =  fails "mkList" t
   and mkArgs (NonTerm("term-list",[a as NonTerm("atom",_),
                                    l as NonTerm("term-list",_)]))
           = (mkTerm a)::(mkArgs l)
     | mkArgs (NonTerm("term-list",[a as NonTerm("atom",_)]))
           = [mkTerm a]
     | mkArgs t = fails "mkArgs" t
   and mkTerms (NonTerm("terms",[a as NonTerm("atom",_),
                                 ts as NonTerm("terms",_)]))
                 = (mkTerm a)::(mkTerms ts)
     | mkTerms (NonTerm("terms",[a as NonTerm("atom",_)]))
                 = [mkTerm a]
     | mkTerms t =  fails "mkTerms" t
   and mkRule (NonTerm("rule",[a as NonTerm("atom",_),
                               ts as NonTerm("terms",_)]))
                 = (mkTerm a, mkTerms ts)
     | mkRule (NonTerm("rule",[a as NonTerm("atom",_)]))
                 = (mkTerm a,[])
     | mkRule t = fails "mkRule" t
   and mkRules (NonTerm("rule-list",[l as NonTerm("rule-list",_),
                                     r as NonTerm("rule",_)]))
         = (mkRule(r))::(mkRules l)
     | mkRules (NonTerm("rule-list",[r as NonTerm("rule",_)]))
         = [mkRule(r)]
     | mkRules t = fails "mkRules" t
in
   val rules = List.rev o mkRules o parserulesq
   val goal = mkTerm o parsetermq
end

local
   fun occurs_in x =
      fn (Var y) => x = y
       | (Fn(_,l)) => List.exists (occurs_in x) l
   fun assoc i =
      (Option.map (fn (_,v) => v)) o (List.find (fn (k,_) => k = i))
   fun subs insts =
     fn (tm as (Var y)) => (case assoc y insts of NONE => tm | SOME v => v)
      | (Fn(s,l)) => Fn(s,List.map (subs insts) l)
   fun augment1 theta (x,s) =
      let val s' = subs theta s
      in
         if occurs_in x s andalso not (s = Var(x))
            then raise Fail "Occurs check."
            else (x,s')
      end
   fun raw_augment p insts = p::(List.map (augment1 [p]) insts)
   fun augment (v,t) insts =
      let val t' = subs insts t
      in case t'
           of Var (w) => 
                if w <= v 
                  then if w = v
                          then insts
                          else raw_augment (v,t') insts
                  else raw_augment (w,Var(v)) insts 
            | _ => if occurs_in v t' 
                      then raise Fail "Occurs check."
                      else raw_augment (v,t') insts
      end
    fun itlist2 f =
      let fun iter [] [] = (fn b => b)
            | iter (h::t) (h'::t') = (fn b => f h h' (itlist2 f t t' b))
            | iter _ _ = raise Fail "Arity."
      in iter
      end
   fun unify tm1 tm2 insts =
      case tm1
        of Var(x) => 
           (case assoc x insts
              of NONE => augment (x,tm2) insts
               | SOME tm1' => unify tm1' tm2 insts)
         | Fn(f1,args1) =>
           (case tm2
              of (Var(y)) => 
                 (case assoc y insts
                    of NONE => augment (y,tm1) insts
                     | SOME tm2' => unify tm1 tm2' insts)
               | Fn(f2,args2) =>
                   if f1 = f2 then itlist2 unify args1 args2 insts
                              else raise Fail ("Constants: mismatch: "^f1^"<>"^f2))
   fun rename s =
      fn (Var v) => Var("~"^v^s)
       | (Fn(f,args)) => Fn(f,List.map (rename s) args)
   fun rename_rule s (conc,assums) =
       (rename s conc,List.map (rename s) assums)
   fun expand n rules insts goals =
      let fun first f =
                (fn [] => raise Fail "No rules apply."
                  | (h::t) => (f h handle Fail _ => first f t))
          fun search rule = 
             if goals = []
                then insts
                else
                   let fun eqgoal (Fn("!=",[a,b])) = not (a = b)
                         | eqgoal _ = false
                       val (conc,assums) = rename_rule (Int.toString n) rule
                       val goal = hd goals
                       val insts' = if eqgoal goal then insts else unify conc goal insts
                       fun occurs (v,_) = occurs_in v conc orelse List.exists (occurs_in v) assums
                       val (loc,glob) = List.partition occurs insts'
                       val goals' = (List.map (subs loc) assums) @ (tl goals)
                   in expand (n+1) rules glob goals'
                   end
      in
         first search rules
      end
in
   fun prolog rules goal =
      let val insts = expand 0 rules [] [goal]
      in Yes (List.filter (fn (v,_) => occurs_in v goal) insts)
      end handle Fail _ => No
end
