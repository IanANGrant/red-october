
fun parseExprPlain file stream lexbuf =
    let val () = CSyntax.reset_parser();
        val expr = CParser.File CPPLexer.Token lexbuf
    in
	Parsing.clearParser();
	expr
    end
    handle exn => (CSyntax.reset_parser();Parsing.clearParser(); raise exn);

fun parseConstExprPlain file stream lexbuf =
    let val expr = CParser.ConstExp CPPLexer.Token lexbuf
    in
	Parsing.clearParser();
	expr
    end
    handle exn => (Parsing.clearParser(); raise exn);

fun parseCPPPlain file stream lexbuf =
    let val () = CSyntax.reset_parser();
        val expr = CParser.CPPFile CPPLexer.Token lexbuf
    in
	Parsing.clearParser();
	expr
    end
    handle exn => (CSyntax.reset_parser();Parsing.clearParser(); raise exn);

fun parseCPPInfo file s lexbuf =
    let val expr = CPPInfoParser.CPPInfo CPPInfoLexer.Token lexbuf
    in
	Parsing.clearParser();
	expr
    end
    handle exn => (Parsing.clearParser(); raise exn);

fun parseCReport Start file stream lexbuf =
    let val () = CSyntax.reset_parser();
        val expr = 
	    Start CPPLexer.Token lexbuf
	    handle
	       Parsing.ParseError f =>
		   let val pos1 = Lexing.getLexemeStart lexbuf
		       val pos2 = Lexing.getLexemeEnd lexbuf
		   in
		       Location2.errMsg (file, stream, lexbuf) 
		                       (Location2.Loc(pos1, pos2))
		                       "Syntax error."
		   end
	     | CPPLexer.LexicalError(msg, pos1, pos2) =>
		   if pos1 >= 0 andalso pos2 >= 0 then
		       Location2.errMsg (file, stream, lexbuf)
		                       (Location2.Loc(pos1, pos2))
		                       ("Lexical error: " ^ msg)
		   else 
		       (Location2.errPrompt ("Lexical error: " ^ msg ^ "\n\n");
			raise Fail "Lexical error");
    in
        CPPLexer.resetLexerState();
	Parsing.clearParser();
	expr
    end
    handle exn => (CSyntax.reset_parser();CPPLexer.resetLexerState();Parsing.clearParser(); raise exn);

val parseExprReport = parseCReport CParser.File;

val parseCPPReport = parseCReport CParser.CPPFile;

fun parseGrammarReport file stream lexbuf =
    let val expr = 
	    GrammarParser.File GrammarLexer.Token lexbuf
	    handle
	       Parsing.ParseError f =>
		   let val pos1 = Lexing.getLexemeStart lexbuf
		       val pos2 = Lexing.getLexemeEnd lexbuf
		   in
		       Location2.errMsg (file, stream, lexbuf)
		                       (Location2.Loc(pos1, pos2))
		                       "Syntax error."
		   end
	     | GrammarLexer.LexicalError(msg, pos1, pos2) =>
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

(* Test Lexer *)

fun printTokens eof lexfn lexbuf = 
   let fun iter acc token = 
          if token = eof
             then rev (token::acc)
             else (iter (token::acc) (lexfn lexbuf))
   in iter [] (lexfn lexbuf)
   end

fun lexExprReport file stream lexbuf =
    let val expr = 
	    printTokens CParser.EOF CPPLexer.Token lexbuf
	    handle
	       CPPLexer.LexicalError(msg, pos1, pos2) =>
		   if pos1 >= 0 andalso pos2 >= 0 then
		       Location2.errMsg (file, stream, lexbuf)
		                       (Location2.Loc(pos1, pos2))
		                       ("Lexical error: " ^ msg)
		   else 
		       (Location2.errPrompt ("Lexical error: " ^ msg ^ "\n\n");
			raise Fail "Lexical error");
    in
	CPPLexer.resetLexerState();
        Parsing.clearParser();
	expr
    end

fun lexExprPlain file stream lexbuf =
    let val () = CSyntax.reset_parser();
        val expr = printTokens CParser.EOF CPPLexer.Token lexbuf
    in
	CPPLexer.resetLexerState();
	Parsing.clearParser();
	expr
    end
    handle exn => raise exn;

fun createLexerStream (is : BasicIO.instream) =
  Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)

fun createLexerString (s : string) =
  Lexing.createLexerString s

(* Parse a program from a file *)

fun processFile pfn file =
    let val is     = Nonstdio.open_in_bin file
        val lexbuf = createLexerStream is
	val expr   = pfn file is lexbuf
	             handle exn => (BasicIO.close_in is; raise exn)
    in 
        BasicIO.close_in is;
	expr
    end

(* Parse a program from a pipe *)

fun processPipe cmd pfn file input =
    let val proc = Unix.execute("/bin/sh", "-c" :: [cmd])
        val (is,os) = Unix.streamsOf proc;
        val lexbuf = Lexing.createLexer
              (fn buff => fn n => Buffered.input is buff 0 n);
        val _ = TextIO.output (os,input)
        val _ = TextIO.flushOut os
        val _ = TextIO.closeOut os
	val expr   = pfn file is lexbuf
	             handle exn => (TextIO.closeIn is; raise exn)
    in 
        TextIO.closeIn is;
	expr
    end

(* Parse a text from a string *)

fun processString pfn s =
   pfn "string" s (createLexerString s)

val parse_grammar = processFile parseGrammarReport

(* Parse C from a file *)

val parse_c = processFile parseExprReport

val parse_c_string = processString parseExprPlain

val parse_c_cexp_string = processString parseConstExprPlain

val parse_cpp = processFile parseCPPReport

val parse_cpp_string = processString parseCPPPlain

val parse_c_pipe = fn cmd => processPipe cmd parseExprPlain

val parse_cpp_pipe = fn cmd => processPipe cmd parseCPPPlain

val tokenise_c = processFile lexExprReport

val tokenise_c_string = processString lexExprPlain

