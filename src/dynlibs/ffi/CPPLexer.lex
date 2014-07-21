{
open CParser TextIO;

exception LexicalError of string * int * int;

datatype lexingMode =
    NORMALlm
  | SOLlm
  | HASH_DEFINElm
  | HASH_DEFINITIONlm
  | HASH_DEFINITION_LWSlm
  | HASH_PARAMETER_DEFINITIONlm
  | CPP_LINElm
  | INTEGER_LITERALlm
  | FLOAT_LITERALlm
  | STRING_LITERALlm;

val dprint = fn s => (); (* (print s); *)

val lexingMode = ref SOLlm;

val modeStack = ref [] : lexingMode list ref;

fun pushLexingMode m =
   (modeStack := ((!lexingMode)::(!modeStack));
    lexingMode := m);

fun popLexingMode () =
   case (!modeStack) 
     of [] => raise Fail "Internal error: Lexer mode stack empty"
      | (m::l) => (modeStack := l;lexingMode := m);

fun resetLexerState () =
  (lexingMode := SOLlm;
   modeStack := []);

(* For nesting comments *)

val comment_depth = ref 0;

val savedLexemeStart = ref 0;

val initial_string_buffer = CharArray.array(256, #"\000")
val string_buff = ref initial_string_buffer
val string_index = ref 0

fun incr ir = ir := (!ir) + 1
fun decr ir = ir := (!ir) - 1

fun reset_string_buffer () =
(
  string_buff := initial_string_buffer;
  string_index := 0;
  ()
)

fun store_string_char c =
  let open CharArray
      val len = CharArray.length (!string_buff)
  in
    if !string_index >= len then
         let val new_buff = array(len * 2, #"\000")
         in copy
             { src = !string_buff, dst = new_buff, di = 0 };
           string_buff := new_buff
         end
    else ();
         update(!string_buff, !string_index, c);
         incr string_index
  end

fun store_string s =
   let fun store_string_chars [] = ()
         | store_string_chars (c::cs) = (store_string_char c; store_string_chars cs)
   in  store_string_chars (String.explode s)
   end

fun get_stored_string () =
  let open CharArraySlice
      val s = vector(slice(!string_buff, 0, SOME (!string_index)))
  in
    string_buff := initial_string_buffer;
    s
  end

fun lexError msg lexbuf =
(
  resetLexerState();
  raise LexicalError(msg, getLexemeStart lexbuf, getLexemeEnd lexbuf)
)

fun constTooLarge msg lexbuf =
(
  resetLexerState();
  lexError (msg ^ " constant is too large") lexbuf
)

prim_val sml_word_of_string    : string -> word = 1 "sml_word_of_dec"
prim_val sml_word_of_hexstring : string -> word = 1 "sml_word_of_hex"

fun notTerminated msg lexbuf =
(
  resetLexerState();
  raise LexicalError (msg ^ " not terminated",
                      !savedLexemeStart, getLexemeEnd lexbuf)
);

exception NotFound of string;

fun lookup table lexbuf =
  let val s = getLexeme lexbuf in
     Hasht.find table s
     handle Subscript => raise NotFound s
  end;

fun hash_list l =
   let val table = (Hasht.new (List.length l) : (string,token) Hasht.t)
   in List.app (fn (str,tok) => Hasht.insert table str tok) l;
      table
   end;

(* The tables of keywords and operators *)

val keywords = [
   ("void", VOID),
   ("int", INT),
   ("char", CHAR),
   ("long", LONG),
   ("short", SHORT),
   ("float", FLOAT),
   ("double", DOUBLE),
   ("signed", SIGNED),
   ("unsigned", UNSIGNED),
   ("_Bool", BOOL),
   ("_Complex", COMPLEX),
   ("sizeof", SIZEOF),
   ("typedef", TYPEDEF),
   ("defined", DEFINED),
   ("extern", EXTERN),
   ("static", STATIC),
   ("auto", AUTO),
   ("register", REGISTER),
   ("_Thread_local", THREAD_LOCAL),
   ("const", CONST),
   ("volatile", VOLATILE),
   ("restrict", RESTRICT),
   ("inline", INLINE),
   ("_Noreturn", NO_RETURN),
   ("_Alignas", ALIGNAS),
   ("_Static_assert", STATIC_ASSERT),
   ("_Atomic", ATOMIC),
   ("struct", STRUCT),
   ("union", UNION),
   ("enum", ENUM),
   ("for", FOR),
   ("while", WHILE),
   ("do", DO),
   ("goto", GOTO),
   ("continue", CONTINUE),
   ("break", BREAK),
   ("return", RETURN),
   ("switch", SWITCH),
   ("if", IF),
   ("else", ELSE),
   ("case", CASE),
   ("default", DEFAULT),
   ("_Generic", GENERIC)];

val operators = [
   ("++", INC),
   ("+=", ADDASSIGN),
   ("||", LOR),
   ("|=", ORASSIGN),
   ("^=", XORASSIGN),
   ("...", ELIPSIS),
   ("/=", DIVIDEASSIGN),
   ("%=", MODASSIGN),
   ("-=", MINUSASSIGN),
   ("->", SPR),
   ("--", DEC),
   ("<<=", LSASSIGN),
   ("<<", LS),
   ("<=", LE),
   (">>=", RSASSIGN),
   (">>", RS),
   (">=", GE),
   ("==", EQ),
   ("!=", NE),
   ("&&", LAND),
   ("&=", ANDASSIGN),
   ("+", PLUS),
   ("/", DIVIDE),
   ("%", MOD),
   ("-", MINUS),
   ("<", LT),
   (">", GT),
   ("=", EQUALS),
   ("!", LNOT),
   ("&", AND),
   ("|", OR),
   ("^", XOR),
   (".", SR)];
 
val keyword_table = hash_list keywords;
val operator_table = hash_list operators;

fun typedef_name s = 
  let val _ = dprint ("typedef_name (maybe IDENTIFIER): "^s^"\n")
  in if CSyntax.is_typedef_name s
        then TYPEDEF_NAME s
        else IDENTIFIER s
  end;

fun keyword lexbuf = 
  let val s = getLexeme lexbuf
      val _ = dprint ("keyword: "^s^"\n")
  in lookup keyword_table lexbuf
        handle NotFound s => typedef_name s
  end;

fun operator lexbuf = 
  let val s = getLexeme lexbuf
      val _ = dprint ("operator: "^s^"\n")
  in lookup operator_table lexbuf
         handle NotFound s  => lexError "Illegal symbol in input" lexbuf
  end;

fun location lexbuf =
  let val s = getLexeme lexbuf
      val _ = dprint ("location: "^s^"\n")
  in CSyntax.location (s, getLexemeStart lexbuf, getLexemeEnd lexbuf)
  end

val char_for_backslash = fn
(* *)    #"n" => #"\010"
(* *)  | #"r" => #"\013"
(* *)  | #"a" => #"\007"
(* *)  | #"b" => #"\008"
(* *)  | #"t" => #"\009"
(* *)  | #"v" => #"\011"
(* *)  | #"f" => #"\012"
(* *)  | c => c
;

fun ishexdigit c = 
   #"0" <= c andalso c <= #"9" orelse
   #"a" <= c andalso c <= #"f" orelse
   #"A" <= c andalso c <= #"F";

fun hexval c =
   if ishexdigit c
      then if #"0" <= c andalso c <= #"9"
              then Char.ord c - 48
              else (Char.ord c - 55) mod 32
   else raise Fail "Internal error: hexval of non-hex digit"

fun intOfString radix offs s =
   let val n = String.size s
       fun iter r i =
           let val c = String.sub(s,i)
           in if ishexdigit c andalso hexval c < radix
                 then
                    let val r =  radix * r + hexval c
                    in if i = n-1 then r else iter r (i + 1)
                    end
                 else r
           end
   in iter 0 offs
   end;

fun skipString msg skip lexbuf =
  let
    val pos1 = getLexemeStart lexbuf
    val pos2 = getLexemeEnd lexbuf
  in
    skip lexbuf;
    resetLexerState();
    raise (LexicalError(msg, pos1, pos2))
  end

fun UTF8StringOfUCSEscapeSequence lexbuf i =
  let
    val s = getLexeme lexbuf
    val sl = String.size s
    fun skipPrefix n =
       let val c = String.sub (s,n)
       in if not (c = #"u" orelse c = #"U" orelse c = #"+") then n else skipPrefix (n+1)
       end
    fun hexCharsToWord n s =
        Word.fromInt (intOfString 16 n s)
  in store_string (UTF8.UCStoUTF8String (hexCharsToWord (skipPrefix 1) s))
  end;

fun scanString scan lexbuf =
(
  reset_string_buffer();
  savedLexemeStart := getLexemeStart lexbuf;
  scan lexbuf;
  setLexStartPos lexbuf (!savedLexemeStart - getLexAbsPos lexbuf)
)

fun getLexbufState lexbuf = 
   let fun str i = Int.toString i
       val sp = str (getLexStartPos lexbuf)
       val cp = str (getLexCurrPos lexbuf)
       val lp = str (getLexLastPos lexbuf)
       val ap = str (getLexAbsPos lexbuf)
   in "startPos="^sp^" currPos="^cp^" lastPos="^lp^" absPos="^ap
   end

}

rule Token = parse
    [^ `\000`-`\255`]
      { lexError "this will be never called!" lexbuf }
  | ""
      { case !lexingMode of
            NORMALlm =>
              (dprint "Normal mode: TokenN\n";TokenN lexbuf)
          | SOLlm =>
              (dprint "SOL mode: TokenSOL\n";TokenSOL lexbuf)
          | CPP_LINElm =>
              (dprint "CPP_LINE mode: CPPLine\n";CPPLine lexbuf)
          | HASH_DEFINElm =>
              HashDefine lexbuf
          | HASH_DEFINITIONlm =>
              HashDefinition lexbuf
          | HASH_DEFINITION_LWSlm =>
              HashDefinitionLWS lexbuf
          | HASH_PARAMETER_DEFINITIONlm =>
              HashParameterDefinition lexbuf
          | INTEGER_LITERALlm =>
              (dprint "INTEGER_LITERAL mode: IntegerLiteral\n";IntegerLiteral lexbuf)
          | STRING_LITERALlm =>
               Token lexbuf
          | FLOAT_LITERALlm =>
              FloatLiteral lexbuf
      }

and TokenSOL = parse
    [` ` `\n` `\r` `\t` `\^L`]          { TokenSOL lexbuf }
  | "#" [` ``\t`]+ [`0`-`9`]+ [^`\n`]*
                                        { location lexbuf; TokenSOL lexbuf } 
  | "#" [` ``\t`]* "define"  [` ``\t`]+
                { lexingMode := HASH_DEFINElm;
                  DEFINE
                }
  | "#" [` ``\t`]* "include" [` ``\t`]+ { CPPInclude lexbuf }
  | "#" [` ``\t`]* "if"   [` ``\t`]+
                { lexingMode := CPP_LINElm;
                  CPPIF
                }
  | "#" [` ``\t`]* "elif"   [` ``\t`]+
                { lexingMode := CPP_LINElm;
                  ELIF
                }
  | "#" [` ``\t`]* "error"  [` ``\t`]+
                { lexingMode := CPP_LINElm;
                  ERROR
                }
  | "#" [` ``\t`]* "warning" [` ``\t`]+
                { lexingMode := CPP_LINElm;
                  WARNING
                }
  | "#" [` ``\t`]* "pragma" [` ``\t`]+
                { lexingMode := CPP_LINElm;
                  PRAGMA
                }
  | "#" [` ``\t`]* "ifdef" [` ``\t`]+
                { lexingMode := CPP_LINElm;
                  IFDEF
                }
  | "#" [` ``\t`]* "ifndef" [` ``\t`]+
                { lexingMode := CPP_LINElm;
                  IFNDEF
                }
  | "#" [` ``\t`]* "undef" [` ``\t`]+
                { lexingMode := CPP_LINElm;
                  UNDEF
                }
  | "#" [` ``\t`]* "else"
                { lexingMode := CPP_LINElm;
                  CPPELSE }
  | "#" [` ``\t`]* "endif"
                { lexingMode := CPP_LINElm;
                  ENDIF }
  | ""          { lexingMode := NORMALlm;
                  TokenN lexbuf
                }

and TokenN = parse
    [`\n`]                            { lexingMode := SOLlm;
                                        TokenSOL lexbuf }
  | [` ` `\r` `\t` `\^L`]             { TokenN lexbuf }
  | [` ` `\t`]* "//" [^`\n`]* `\n`    { lexingMode := SOLlm;
                                        TokenSOL lexbuf }
  | "/*"      { savedLexemeStart := getLexemeStart lexbuf;
                comment_depth := 1; Comment lexbuf; TokenN lexbuf
              }
  | "*/"      { lexError "unmatched comment bracket" lexbuf }
  | (eof | `\^Z`) { lexingMode := SOLlm;
                    EOF }
  | ""        { dprint ("TokenN: lexbuf state is: "^(getLexbufState lexbuf)^"\n");
                Tokens lexbuf }

and Tokens = parse
    [`a`-`z``A`-`Z` `_`][`a`-`z``A`-`Z``0`-`9` `_`]*
        { keyword lexbuf } 
  | [`+``/``%``-``<``>``=``!``&``|``?``^``.`]+
        { operator lexbuf }
  | "*=" { MULTIPLYASSIGN }
  | `*` { STAR }
  | `~` { NOT }
  | `$` { NI }
  | `(` { LPAR }
  | `)` { RPAR }
  | `{` { LBRACE }
  | `}` { RBRACE }
  | `[` { LBRACK }
  | `]` { dprint ("Tokens: RBRACK\n");
                 RBRACK }
  | `;` { SEMICOLON }
  | `?` { THEN }
  | `:` { COLON }
  | `,` { dprint ("Tokens: COMMA\n");
                 COMMA }
  | ( ( [`0`-`9`]+ ) [`e``E`] | [`0`-`9`]+ `.` )   
                { pushLexingMode FLOAT_LITERALlm;
                  rewindLexbuf lexbuf;
                  FloatLiteral lexbuf
                }
  | ([`0`-`9`]+ | ("0" [`x``X`] [`0`-`9``a`-`f``A`-`F`]+)) 
                { dprint"Tokens: integer. pushing lexing mode to INTEGER_LITERALlm\n";
                  pushLexingMode INTEGER_LITERALlm;
                  dprint ("Tokens: lexbuf state is: "^(getLexbufState lexbuf)^"\n");
                  dprint "Tokens: rewinding buffer.\n";
                  rewindLexbuf lexbuf;
                  dprint ("Tokens: lexbuf state is: "^(getLexbufState lexbuf)^"\n");
                  dprint"Tokens: Now going to IntegerLiteral state machine.\n";
                  IntegerLiteral lexbuf
                }
  | `L`? "\""
      { scanString String lexbuf;
        STRING (get_stored_string())
      }
  | `L`? "'"
      { scanString Char lexbuf;
        let val s = get_stored_string() in
          if size s <> 1 then
            lexError "ill-formed character constant" lexbuf
          else ();
          CHARACTER (String.str (CharVector.sub(s, 0)))
        end }
  | _ { lexError "unexpected character" lexbuf }

and FloatLiteral = parse
   [`0`-`9`]+     { DIGIT_SEQUENCE (getLexeme lexbuf) }
 | [`e``E`]       { E }
 | [`.`]          { DECIMAL_POINT }
 | [`+``-`]       { SIGN (getLexeme lexbuf) }
 | [`l``L``f``F`] { FLOATING_SUFFIX (getLexeme lexbuf) }
 | ""              { popLexingMode();
                    Token lexbuf }

and IntegerLiteral = parse
    "0" [`0`-`7`]+                { dprint ("IntegerLiteral: got OCTINT "^(getLexeme lexbuf)^"\n");
                                    OCTINT (getLexeme lexbuf) }
  | [`1`-`9`] [`0`-`9`]*          { dprint ("IntegerLiteral: got DECINT "^(getLexeme lexbuf)^"\n");
                                    DECINT (getLexeme lexbuf) }
  | "0" [`X``x`]
        [`0`-`9``a`-`f``A`-`F`]
        [`0`-`9``a`-`f``A`-`F`]*  { dprint ("IntegerLiteral: got HEXINT "^(getLexeme lexbuf)^"\n");
                                    HEXINT (getLexeme lexbuf) }
  | "0"                           { DECINT (getLexeme lexbuf) }
  | [`u``U`]                      { UNSIGNED_SUFFIX }
  | [`l``L`]                      { LONG_SUFFIX }
  | ""                             {dprint("IntegerLiteral: got \""^(getLexeme lexbuf)^"\", popping mode\n");
                                    popLexingMode();
                                    dprint("IntegerLiteral: lexbuf state is: "^(getLexbufState lexbuf)^"\n");
                                    Token lexbuf }

and HashDefine = parse
    [` ``\t`]                       { HashDefine lexbuf }
  | [`a`-`z``A`-`Z``_`]
    [`a`-`z``A`-`Z``0`-`9``_`]* `(` { let val s' = getLexeme lexbuf (*'*)
                                          val s = String.substring (s',0,(String.size s') - 1)
                                      in lexingMode := HASH_PARAMETER_DEFINITIONlm;
                                         PMIDENTIFIER s
                                      end }
  | [`a`-`z``A`-`Z``_`][`a`-`z``A`-`Z``0`-`9``_`]* { lexingMode := HASH_DEFINITION_LWSlm;
                                                     IDENTIFIER (getLexeme lexbuf) }
  | _ { lexError "expected an identifier" lexbuf }

and HashParameterDefinition = parse
   `\n`      { lexingMode := SOLlm;
               EOL }
  | [` ``\t`]                   { HashParameterDefinition lexbuf }
  | `)`                         { lexingMode := HASH_DEFINITION_LWSlm;
                                  RPAR }
  | `,`                         { COMMA }
  | [`a`-`z``A`-`Z``_`]
    [`a`-`z``A`-`Z``0`-`9``_`]* { IDENTIFIER (getLexeme lexbuf) }
  | "..." { IDENTIFIER (getLexeme lexbuf) }
  | _ { lexError "expected an identifier, or ')'" lexbuf }

and HashDefinitionLWS = parse
   `\n`      { lexingMode := SOLlm;
               EOL }
  | [` ``\t`] { HashDefinitionLWS lexbuf }
  | ""        { lexingMode := HASH_DEFINITIONlm;
                Token lexbuf }

and HashDefinition = parse
   `\n`      { lexingMode := SOLlm;
               EOL }
  | "/*"     { savedLexemeStart := getLexemeStart lexbuf;
               comment_depth := 1; CommentNoNewLine lexbuf
             }
  | "*/"                        { lexError "unmatched comment bracket" lexbuf }
  | [`a`-`z``A`-`Z``_`][`a`-`z``A`-`Z``0`-`9``_`]* { IDENTIFIER (getLexeme lexbuf) }
  | "##"                        { TOKEN_PASTE }
  | [^`\n`]+                { TEXT_FRAG (getLexeme lexbuf) } (* FIXME: only works for one-line defns. *)
  | "\\\n"   { HashDefinition lexbuf }
  | _ { lexError "ill-formed preprocessor definition" lexbuf }

and CPPLine = parse
    [` ``\t`] { CPPLine lexbuf } 
  | `\n`      { lexingMode := SOLlm;
                EOL }
  | "\\\n"    { CPPLine lexbuf }
  | "//" [^`\n`]* `\n`    { lexingMode := SOLlm;
                            EOL }
  | "/*"
              { savedLexemeStart := getLexemeStart lexbuf;
                comment_depth := 1; CommentNoNewLine lexbuf
              }
  | "*/"
              { lexError "unmatched comment bracket" lexbuf }
  | ""        { Tokens lexbuf }

and CPPInclude = parse
    [` ``\t`] { CPPInclude lexbuf } 
  | `\n`      { lexingMode := SOLlm;
                EOL }
  | "\\\n"    { CPPInclude lexbuf }
  | "//" [^`\n`]* `\n`    { lexingMode := SOLlm;
                            EOL }
  | "/*"
              { savedLexemeStart := getLexemeStart lexbuf;
                comment_depth := 1; CommentNoNewLine lexbuf
              }
  | "*/"
              { lexError "unmatched comment bracket" lexbuf }
  | "\""
      { scanString String lexbuf;
        INCLUDE (get_stored_string())
      }
  | "<"
      { dprint ("#include: got \"<\"\n");
        scanString SysInclude lexbuf;
        SYSINCLUDE (get_stored_string())
      }
  | _
      { lexError "unexpected character on #include line" lexbuf }

and CommentNoNewLine = parse
    "/*"
      { (incr comment_depth; CommentNoNewLine lexbuf) }
  | "*/"
      { (decr comment_depth;
         if !comment_depth > 0 then CommentNoNewLine lexbuf else Token lexbuf) }
  | [`\n`] { lexingMode := SOLlm;
             EOL }
  | (eof | `\^Z`)
      { notTerminated "comment" lexbuf }
  | _
      { CommentNoNewLine lexbuf }

and Comment = parse
    "/*"
      { (incr comment_depth; Comment lexbuf) }
  | "*/"
      { (decr comment_depth;
         if !comment_depth > 0 then Comment lexbuf else ()) }
  | (eof | `\^Z`)
      { notTerminated "comment" lexbuf }
  | _
      { Comment lexbuf }

and UTF8Char = parse
   [`\^@`-`\127`] { let val c = getLexemeChar lexbuf 0
                    in if c = #">" then dprint "UTF8Char: got \">\"\n" else ();
                       store_string_char c
                    end }
 | [`\194`-`\223`] [`\128`-`\191`] { store_string_char(getLexemeChar lexbuf 0);
                                     store_string_char(getLexemeChar lexbuf 1) }
 | `\224` [`\160`-`\191`] [`\128`-`\191`] { store_string_char(getLexemeChar lexbuf 0);
                                            store_string_char(getLexemeChar lexbuf 1);
                                            store_string_char(getLexemeChar lexbuf 2) }
 | [`\225`-`\236`] [`\128`-`\191`] [`\128`-`\191`] { store_string_char(getLexemeChar lexbuf 0);
                                                     store_string_char(getLexemeChar lexbuf 1);
                                                     store_string_char(getLexemeChar lexbuf 2) }
 | `\237` [`\128`-`\159`] [`\128`-`\191`] { store_string_char(getLexemeChar lexbuf 0);
                                            store_string_char(getLexemeChar lexbuf 1);
                                            store_string_char(getLexemeChar lexbuf 2) }
 | [`\238``\239`] [`\128`-`\191`] [`\128`-`\191`] { store_string_char(getLexemeChar lexbuf 0);
                                                    store_string_char(getLexemeChar lexbuf 1);
                                                    store_string_char(getLexemeChar lexbuf 2) }
 | `\240` [`\144`-`\191`] [`\128`-`\191`] [`\128`-`\191`] { store_string_char(getLexemeChar lexbuf 0);
                                                            store_string_char(getLexemeChar lexbuf 1);
                                                            store_string_char(getLexemeChar lexbuf 2);
                                                            store_string_char(getLexemeChar lexbuf 3) }
 | [`\241`-`\243`] [`\128`-`\191`] [`\128`-`\191`] [`\128`-`\191`]
                                                          { store_string_char(getLexemeChar lexbuf 0);
                                                            store_string_char(getLexemeChar lexbuf 1);
                                                            store_string_char(getLexemeChar lexbuf 2);
                                                            store_string_char(getLexemeChar lexbuf 3) }
 | `\244` [`\128`-`\143`] [`\128`-`\191`] [`\128`-`\191`] { store_string_char(getLexemeChar lexbuf 0);
                                                            store_string_char(getLexemeChar lexbuf 1);
                                                            store_string_char(getLexemeChar lexbuf 2);
                                                            store_string_char(getLexemeChar lexbuf 3) }
  | _
      { lexError "ill-formed UTF8 character code" lexbuf }

and String = parse
    `"` (* " *)
      { () }
  | `\\` [`\\` `"` `'` `a` `b` `t` `n` `v` `r` `f` `?`] (*"*)
      { store_string_char(char_for_backslash(getLexemeChar lexbuf 1));
        String lexbuf }
  | `\\` ([`0`-`3`]? [`0`-`7`] )? [`0`-`7`]
      { let val code = intOfString 8 1 (getLexeme lexbuf) in
          store_string_char(Char.chr code);
          String lexbuf
        end }
  | `\\` [`x``X`] [`0`-`9``a`-`f``A`-`F`]? [`0`-`9``a`-`f``A`-`F`]
      { let val code = intOfString 16 2 (getLexeme lexbuf) in
          store_string_char(Char.chr code);
          String lexbuf
        end }
  | `\\` [`u``U`]? `+`? [`0`-`9``a`-`f``A`-`F`]? [`0`-`9``a`-`f``A`-`F`]? 
         [`0`-`9``a`-`f``A`-`F`] [`0`-`9``a`-`f``A`-`F`] 
         [`0`-`9``a`-`f``A`-`F`] [`0`-`9``a`-`f``A`-`F`]
      { UTF8StringOfUCSEscapeSequence lexbuf 1
        handle UTF8.BadUTF8 s => skipString s SkipString lexbuf;
        String lexbuf }
  | `\\`
      { skipString "ill-formed string escape sequence" SkipString lexbuf }
  | (eof | `\^Z`)
      { notTerminated "string" lexbuf }
  | [`\n` `\r`]
      { skipString "newline not permitted in string" SkipString lexbuf }
  | "" { UTF8Char lexbuf;
         String lexbuf }

and SysInclude = parse
    ">"
      { () }
  | `\\` [`\\` `"` `'` `a` `b` `t` `n` `v` `r` `f` `?` `>`] (* " *)
      { let val c = getLexemeChar lexbuf 1
        in if c = #">" then store_string_char c 
                       else store_string_char(char_for_backslash c);
           SysInclude lexbuf
        end }
  | `\\` ([`0`-`3`]? [`0`-`7`] )? [`0`-`7`]
      { let val code = intOfString 8 1 (getLexeme lexbuf) in
          store_string_char(Char.chr code);
          SysInclude lexbuf
        end }
  | `\\` [`x``X`] [`0`-`9``a`-`f``A`-`F`]? [`0`-`9``a`-`f``A`-`F`]
      { let val code = intOfString 16 2 (getLexeme lexbuf) in
          store_string_char(Char.chr code);
          SysInclude lexbuf
        end }
  | `\\` [`u``U`]? `+`? [`0`-`9``a`-`f``A`-`F`]? [`0`-`9``a`-`f``A`-`F`]? 
         [`0`-`9``a`-`f``A`-`F`] [`0`-`9``a`-`f``A`-`F`] 
         [`0`-`9``a`-`f``A`-`F`] [`0`-`9``a`-`f``A`-`F`]
      { UTF8StringOfUCSEscapeSequence lexbuf 1
        handle UTF8.BadUTF8 s => skipString s SkipString lexbuf;
        SysInclude lexbuf }
  | `\\`
      { skipString "ill-formed string escape sequence" SkipString lexbuf }
  | (eof | `\^Z`)
      { notTerminated "string" lexbuf }
  | [`\n` `\r`]
      { skipString "newline not permitted in string" SkipString lexbuf }
  | "" { UTF8Char lexbuf;
         SysInclude lexbuf }

and Char = parse
    `'`
      { () }
  | `\\` [`\\` `'` `"` `a` `b` `t` `n` `v` `r` `f` `?`] (* " *)
      { store_string_char(char_for_backslash(getLexemeChar lexbuf 1));
        Char lexbuf }
  | `\\` ([`0`-`3`]? [`0`-`7`] )? [`0`-`7`]
      { let val code = intOfString 8 1 (getLexeme lexbuf) in
          store_string_char(Char.chr code);
          Char lexbuf
        end }
  | `\\` [`x``X`] [`0`-`9``a`-`f``A`-`F`]? [`0`-`9``a`-`f``A`-`F`]
      { let val code = intOfString 16 2 (getLexeme lexbuf) in
          store_string_char(Char.chr code);
          Char lexbuf
        end }
  | `\\`
      { skipString "ill-formed character escape sequence" SkipChar lexbuf }
  | (eof | `\^Z`)
      { notTerminated "character" lexbuf }
  | [`\n` `\r`]
      { skipString "newline not permitted in character" SkipChar lexbuf }
  | "" { UTF8Char lexbuf;
         Char lexbuf }

and SkipString = parse
    `"`
      { () }
  | `\\` [`\\` `"`]
      { SkipString lexbuf }
  | (eof | `\^Z`)
      { notTerminated "string" lexbuf }
  | _
      { SkipString lexbuf }

and SkipChar = parse
    `'`
      { () }
  | `\\` [`\\` `'`]
      { SkipChar lexbuf }
  | (eof | `\^Z`)
      { notTerminated "character" lexbuf }
  | _
      { SkipChar lexbuf }
;
