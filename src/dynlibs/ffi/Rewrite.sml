open GrammarSyntax

fun printRewrites t =
   case t 
     of (NonTerm("rhs-meta-quote",[Term(v)])) =>
           "⌜"^v^"⌝"
      | (NonTerm("rhs-meta-quote",[NonTerm("",[])])) =>
           "⌜ε⌝"
      | (NonTerm("rhs-meta-quote",[t])) =>
           "⟦"^(printRewrites t)^"⟧"
      | (NonTerm("rhs-meta-quote",[Term(v),t as NonTerm("rule-rhs",_)])) =>
           "⌜"^v^"("^(printRewrites t)^")⌝"
      | (NonTerm("rhs-arg",[t as NonTerm("rhs-meta-quote",_)])) =>
           printRewrites t
      | (NonTerm("rhs-arg",[t as NonTerm("rule-rhs",_)])) =>
           "("^(printRewrites t)^")"
      | (NonTerm("rhs-arg",[t as NonTerm("at-arg",_)])) =>
           printRewrites t
      | (NonTerm("at-arg",[t])) =>
           printRewrites t
      | (NonTerm("identifier",[Term(s)])) =>
           s
      | (NonTerm("literal",[Term(s)])) =>
           s
      | (NonTerm("rhs-arg-list",[t as NonTerm("rhs-arg",_)])) =>
           printRewrites t
      | (NonTerm("rhs-arg-list",[l,t as NonTerm("rhs-arg",_)])) =>
           (printRewrites l)^" "^(printRewrites t)
      | (NonTerm("lhs-arg-list",[t as NonTerm("lhs-arg",_)])) =>
           printRewrites t
      | (NonTerm("lhs-arg-list",[l,t as NonTerm("lhs-arg",_)])) =>
           (printRewrites l)^" "^(printRewrites t)
      | (NonTerm("lhs-arg",[l as (NonTerm("rule-lhs",_))])) =>
           "("^(printRewrites l)^")"
      | (NonTerm("lhs-arg",[t as NonTerm("lhs-metaquote",_)])) =>
           printRewrites t
      | (NonTerm("lhs-metaquote",[Term(v)])) =>
           "⌜"^v^"⌝"
      | (NonTerm("lhs-metaquote",[Term(v),Term(t)])) =>
           "⌜"^v^":"^t^"⌝"
      | (NonTerm("lhs-metaquote",[Term(v),t as NonTerm("rule-lhs",_)])) =>
           "⌜"^v^":=("^(printRewrites t)^")⌝"
      | (NonTerm("lhs-arg",[t as NonTerm("at-arg",_)])) =>
           printRewrites t
      | (NonTerm("rule-list",[t as NonTerm("rule",_)])) =>
           printRewrites t
      | (NonTerm("rule-list",[l,t as NonTerm("rule",_)])) =>
           (printRewrites l)^(printRewrites t)
      | (NonTerm("rule",[t,t'])) =>
           "⟦"^(printRewrites t)^"⟧ = ("^(printRewrites t')^")\n"
      | (NonTerm("rule-rhs",[Term(s),l])) =>
            s^" "^(printRewrites l)
      | (NonTerm("rule-rhs",[Term(s)])) =>
            s
      | (NonTerm("rule-rhs",[l])) =>
            printRewrites l
      | (NonTerm("rule-lhs",[Term(s)])) =>
            s
      | (NonTerm("rule-lhs",[Term(s),l])) =>
            s^" "^(printRewrites l)
      | (NonTerm("rule-lhs",[l as NonTerm("lhs-metaquote",_)])) =>
            printRewrites l
      | (NonTerm(s,l)) => (printTree t;raise Fail ("printRewrites: unknown nonterminal: "^s))
      | (Term(s)) => raise Fail ("printRewrites: unknown terminal: "^s);

fun match l = tree_unify_match (fn (_,_,a) => a) l;

fun antiquote l =
   NonTerm("%aquote",l);

fun epsilon () =
   NonTerm("%",[]);

fun funsubs l =
   NonTerm("%funsubs",l);

fun quote s =
   NonTerm("%quote",[Term(s)]);

val fsubs = fn f => fn v => fn (t,l) => f (t,(v,l));

val subs = 
  fsubs (fn (tree,(var,(acc,m:bool))) =>
    (if var = "_" then acc else (NonTerm(var,[tree]))::acc,m));

fun matchsubs name = 
  fsubs (fn (tree,(var,(acc,m:bool))) =>
   let 
   in case tree 
        of (NonTerm(n,_)) =>
             if name="*" orelse n=name
                then (if var = "_" then acc else (NonTerm(var,[tree]))::acc,m)
                else (acc,false)
         | _ => (acc,false)
   end);

fun asmatch l = 
  fsubs (fn (tree,(var,(acc,m:bool))) =>
   let val (acc',m') = tree_unify_match (fn (_,_,a) => a) l 
                                        (tree, (if var = "_" then acc else (NonTerm(var,[tree]))::acc,m))
   in if m'
         then (if var = "_" then acc' else (NonTerm(var,[tree]))::acc',m')
         else (acc,false)
   end);

fun matchterm_lit value = 
  fsubs (fn (tree,(var,(acc,m:bool))) =>
   let fun stripq s = Substring.string (Substring.triml 1 (Substring.trimr 1 (Substring.full s)))
       val value' = stripq value
    (* val _ = print ("debug: matching terminal "^value'^" "^value^"\n") *)
   in case tree 
        of (NonTerm(_,_)) => (acc,false)
         | (Term(s)) =>
            let val s' = stripq s
             (* val _ = print ("debug: matching terminal "^s'^" was "^s^"\n") *)
            in
              if s'=value'
                 then ((NonTerm(var,[tree]))::acc,m)
                 else (acc,false)
            end
   end) "_";

fun matchterm_id value = 
  fsubs (fn (tree,(var,(acc,m:bool))) =>
   let (* val _ = print ("debug: matching identifier "^value^"\n") *)
   in case tree 
        of (Term(s)) =>
            let (* val _ = print ("debug: matching identifier "^s^"\n") *)
            in
              if s=value
                 then ((NonTerm(var,[tree]))::acc,m)
                 else (acc,false)
            end
          | _ => (acc,false)
   end) "_";

fun compileActiveRewrites t =
   case t 
     of (NonTerm("rule-list",[t as NonTerm("rule",_)])) =>
           compileActiveRewrites t
      | (NonTerm("rule-list",[l,t as NonTerm("rule",_)])) =>
           (compileActiveRewrites l)@(compileActiveRewrites t)
      | (NonTerm("rule",[NonTerm("rule-lhs",[Term(s),l]),t])) =>
           [((s, compileLhsArgList l),compileRhs t)]
      | (NonTerm("rule",[NonTerm("rule-lhs",[l as NonTerm("lhs-metaquote",[Term(v),Term(t')])]),t])) =>
           [((t', [matchsubs t' v]),compileRhs t)]
      | (NonTerm("rule",[NonTerm("rule-lhs",[NonTerm("lhs-metaquote",[Term(v)])]),t])) =>
           [(("*", [matchsubs "*" v]),compileRhs t)]
      | (NonTerm("rule",[NonTerm("rule-lhs",
                                 [NonTerm("lhs-metaquote",
                                          [Term(v),NonTerm("rule-lhs",[Term(s),l])])]),t])) =>
                raise Fail ("compileRewrites: ⌜x:=(...)⌝ patterns can't be used here, sorry")
      | (NonTerm(s,l)) => (printTree t;raise Fail ("compile_rewrites: unknown nonterminal: "^s))
      | (Term(s)) => raise Fail ("compile_rewrites: unknown terminal: "^s)
and compileLhs t =
   case t
     of (NonTerm("lhs-arg",[l as NonTerm("rule-lhs",_)])) =>
           compileLhs l
      | (NonTerm("lhs-arg",[l as NonTerm("lhs-metaquote",_)])) =>
           compileLhs l
      | (NonTerm("rule-lhs",[Term(s),l])) =>
            match [((s, compileLhsArgList l),Term "")]
      | (NonTerm("lhs-metaquote",[Term(v)])) =>
           subs v
      | (NonTerm("lhs-metaquote",[Term(v),Term(t)])) =>
           matchsubs t v
      | (NonTerm("lhs-metaquote",[Term(v),NonTerm("rule-lhs",[Term(s),l])])) =>
           asmatch [((s, compileLhsArgList l),Term "")] v
      | (NonTerm("rule-lhs",[Term(s)])) =>
            match [((s,[]),Term "")]
      | (NonTerm("lhs-arg",[t as NonTerm("at-arg",[NonTerm("identifier",[Term(s)])])])) =>
           matchterm_id s (* match [((s, []),Term "")] *)
      | (NonTerm("lhs-arg",[t as NonTerm("at-arg",[NonTerm("literal",[Term(s)])])])) =>
           matchterm_lit s
      | (NonTerm(s,l)) => (printTreeDirect 0 t;raise Fail ("compile_lhs: unknown lhs nonterminal: "^s))
      | (Term(s)) => raise Fail ("compile_lhs: unknown lhs terminal: "^s)
and compileLhsArgList t =
   case t 
     of (NonTerm("lhs-arg-list",[t as NonTerm("lhs-arg",_)])) =>
           [compileLhs t]
      | (NonTerm("lhs-arg-list",[l,t as NonTerm("lhs-arg",_)])) =>
           (compileLhsArgList l) @ [compileLhs t]
      | (NonTerm(s,l)) => raise Fail ("unknown lhs-arg-list nonterminal: "^s)
      | (Term(s)) => raise Fail ("unknown lhs-arg-list terminal: "^s)
and compileRhs t =
   case t 
     of (NonTerm("rule-rhs",[Term(s),l])) =>
           NonTerm(s,compileRhsArgList l)
      | (NonTerm("rule-rhs",[Term(s)])) =>
           NonTerm(s,[])
      | (NonTerm("rule-rhs",[l])) =>
           compileRhs l
      | (NonTerm("rhs-arg",[t])) =>
           compileRhs t
      | (NonTerm("rhs-meta-quote",[Term(v)])) =>
           quote v
      | (NonTerm("rhs-meta-quote",[t as NonTerm("",[])])) =>
           epsilon ()
      | (NonTerm("rhs-meta-quote",[t])) =>
           antiquote [compileRhs t]
      | (NonTerm("rhs-meta-quote",[v as Term(_),t])) =>
           funsubs [v,compileRhs t]
      | (NonTerm("at-arg",[t])) =>
           compileRhs t
      | (NonTerm("identifier",[Term(s)])) =>
           NonTerm(s,[])
      | (NonTerm("literal",[Term(s)])) =>
           Term(s)
      | (NonTerm(s,l)) => raise Fail ("unknown rhs nonterminal: "^s)
      | (Term(s)) => raise Fail ("unknown rhs terminal: "^s)
and compileRhsArgList t =
   case t 
     of (NonTerm("rhs-arg-list",[t as NonTerm("rhs-arg",_)])) =>
           [compileRhs t]
      | (NonTerm("rhs-arg-list",[l,t as NonTerm("rhs-arg",_)])) =>
           (compileRhsArgList l) @ [compileRhs t]
      | (NonTerm(s,l)) => raise Fail ("compileRewrites: unknown rhs-arg-list nonterminal: "^s)
      | (Term(s)) => raise Fail ("compileRewrites: unknown rhs-arg-list terminal: "^s);

val compileRewrites = compileActiveRewrites;

fun createLexerStream (is : BasicIO.instream) =
  Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n);

fun createLexerString (s : string) =
  Lexing.createLexerString s;

fun processString pfn s =
   pfn "string" s (createLexerString s);

fun processFile pfn file =
    let val is     = Nonstdio.open_in_bin file
        val lexbuf = createLexerStream is
	val expr   = pfn file is lexbuf
	             handle exn => (BasicIO.close_in is; raise exn)
    in 
        BasicIO.close_in is;
	expr
    end;

fun parseRewritePlain file stream lexbuf =
    let val expr = RewriteParser.File RewriteLexer.Token lexbuf
    in
	Parsing.clearParser();
	expr
    end
    handle exn => (Parsing.clearParser(); raise exn);

fun parseRewriteReport file stream lexbuf =
    let val expr = 
	    RewriteParser.File RewriteLexer.Token lexbuf
	    handle
	       Parsing.ParseError f =>
		   let val pos1 = Lexing.getLexemeStart lexbuf
		       val pos2 = Lexing.getLexemeEnd lexbuf
		   in
		       Location2.errMsg (file, stream, lexbuf)
		                       (Location2.Loc(pos1, pos2))
		                       "Syntax error."
		   end
	     | RewriteLexer.LexicalError(msg, pos1, pos2) =>
		   if pos1 >= 0 andalso pos2 >= 0 then
		       Location2.errMsg (file, stream, lexbuf)
		                       (Location2.Loc(pos1, pos2))
		                       ("Lexical error: " ^ msg)
		   else 
		       (Location2.errPrompt ("Lexical error: " ^ msg ^ "\n\n");
			raise Fail "Lexical error");
    in
	Parsing.clearParser();
	expr
    end
    handle exn => (Parsing.clearParser(); raise exn);

(* Parse rewrites from a file *)

val parse_rewrites = processFile parseRewriteReport;

(* Parse rewrites from a string *)

val parse_rewrites_string = processString parseRewritePlain;
