datatype term =
   Fn of string * term list
 | Var of string

local open GrammarSyntax
   fun parser startsymbol = 
       let fun parse file stream lexbuf =
          let val expr = startsymbol PrologLexer.Token lexbuf
          in Parsing.clearParser();
   	     expr
          end handle exn => (Parsing.clearParser(); raise exn)
       in parse end
   fun processString pfn = fn s =>
      pfn "string" s (Lexing.createLexerString s)
   fun qlToString l =
      let fun iter r [] = r
            | iter r ((QUOTE s)::fs) = iter (r^s) fs
            | iter r ((ANTIQUOTE s)::fs) = iter (r^s) fs
      in iter "" l
      end
   val parserulesq = processString (parser PrologParser.File) o qlToString
   val parsetermq = processString (parser PrologParser.OneTerm) o qlToString
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
     | mkList (NonTerm("list",[a as NonTerm("atom",_),
                               a' as NonTerm("atom",_)]))
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
   fun mkTerms (NonTerm("terms",[a as NonTerm("atom",_),
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
