local open Obj Lexing in


open CParser UTF8 TextIO;

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
  in store_string (UCStoUTF8String (hexCharsToWord (skipPrefix 1) s))
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


fun action_148 lexbuf = (
 case !lexingMode of
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
      )
and action_147 lexbuf = (
 lexError "this will be never called!" lexbuf )
and action_146 lexbuf = (
 lexingMode := NORMALlm;
                  TokenN lexbuf
                )
and action_145 lexbuf = (
 lexingMode := CPP_LINElm;
                  ENDIF )
and action_144 lexbuf = (
 lexingMode := CPP_LINElm;
                  CPPELSE )
and action_143 lexbuf = (
 lexingMode := CPP_LINElm;
                  UNDEF
                )
and action_142 lexbuf = (
 lexingMode := CPP_LINElm;
                  IFNDEF
                )
and action_141 lexbuf = (
 lexingMode := CPP_LINElm;
                  IFDEF
                )
and action_140 lexbuf = (
 lexingMode := CPP_LINElm;
                  PRAGMA
                )
and action_139 lexbuf = (
 lexingMode := CPP_LINElm;
                  WARNING
                )
and action_138 lexbuf = (
 lexingMode := CPP_LINElm;
                  ERROR
                )
and action_137 lexbuf = (
 lexingMode := CPP_LINElm;
                  ELIF
                )
and action_136 lexbuf = (
 lexingMode := CPP_LINElm;
                  CPPIF
                )
and action_135 lexbuf = (
 CPPInclude lexbuf )
and action_134 lexbuf = (
 lexingMode := HASH_DEFINElm;
                  DEFINE
                )
and action_133 lexbuf = (
 location lexbuf; TokenSOL lexbuf )
and action_132 lexbuf = (
 TokenSOL lexbuf )
and action_131 lexbuf = (
 dprint ("TokenN: lexbuf state is: "^(getLexbufState lexbuf)^"\n");
                Tokens lexbuf )
and action_130 lexbuf = (
 lexingMode := SOLlm;
                    EOF )
and action_129 lexbuf = (
 lexError "unmatched comment bracket" lexbuf )
and action_128 lexbuf = (
 savedLexemeStart := getLexemeStart lexbuf;
                comment_depth := 1; Comment lexbuf; TokenN lexbuf
              )
and action_127 lexbuf = (
 lexingMode := SOLlm;
                                        TokenSOL lexbuf )
and action_126 lexbuf = (
 TokenN lexbuf )
and action_125 lexbuf = (
 lexingMode := SOLlm;
                                        TokenSOL lexbuf )
and action_124 lexbuf = (
 lexError "unexpected character" lexbuf )
and action_123 lexbuf = (
 scanString Char lexbuf;
        let val s = get_stored_string() in
          if size s <> 1 then
            lexError "ill-formed character constant" lexbuf
          else ();
          CHARACTER (String.str (CharVector.sub(s, 0)))
        end )
and action_122 lexbuf = (
 scanString String lexbuf;
        STRING (get_stored_string())
      )
and action_121 lexbuf = (
 dprint"Tokens: integer. pushing lexing mode to INTEGER_LITERALlm\n";
                  pushLexingMode INTEGER_LITERALlm;
                  dprint ("Tokens: lexbuf state is: "^(getLexbufState lexbuf)^"\n");
                  dprint "Tokens: rewinding buffer.\n";
                  rewindLexbuf lexbuf;
                  dprint ("Tokens: lexbuf state is: "^(getLexbufState lexbuf)^"\n");
                  dprint"Tokens: Now going to IntegerLiteral state machine.\n";
                  IntegerLiteral lexbuf
                )
and action_120 lexbuf = (
 pushLexingMode FLOAT_LITERALlm;
                  rewindLexbuf lexbuf;
                  FloatLiteral lexbuf
                )
and action_119 lexbuf = (
 dprint ("Tokens: COMMA\n");
                 COMMA )
and action_118 lexbuf = (
 COLON )
and action_117 lexbuf = (
 THEN )
and action_116 lexbuf = (
 SEMICOLON )
and action_115 lexbuf = (
 dprint ("Tokens: RBRACK\n");
                 RBRACK )
and action_114 lexbuf = (
 LBRACK )
and action_113 lexbuf = (
 RBRACE )
and action_112 lexbuf = (
 LBRACE )
and action_111 lexbuf = (
 RPAR )
and action_110 lexbuf = (
 LPAR )
and action_109 lexbuf = (
 NI )
and action_108 lexbuf = (
 NOT )
and action_107 lexbuf = (
 STAR )
and action_106 lexbuf = (
 MULTIPLYASSIGN )
and action_105 lexbuf = (
 operator lexbuf )
and action_104 lexbuf = (
 keyword lexbuf )
and action_103 lexbuf = (
 popLexingMode();
                    Token lexbuf )
and action_102 lexbuf = (
 FLOATING_SUFFIX (getLexeme lexbuf) )
and action_101 lexbuf = (
 SIGN (getLexeme lexbuf) )
and action_100 lexbuf = (
 DECIMAL_POINT )
and action_99 lexbuf = (
 E )
and action_98 lexbuf = (
 DIGIT_SEQUENCE (getLexeme lexbuf) )
and action_97 lexbuf = (
dprint("IntegerLiteral: got \""^(getLexeme lexbuf)^"\", popping mode\n");
                                    popLexingMode();
                                    dprint("IntegerLiteral: lexbuf state is: "^(getLexbufState lexbuf)^"\n");
                                    Token lexbuf )
and action_96 lexbuf = (
 LONG_SUFFIX )
and action_95 lexbuf = (
 UNSIGNED_SUFFIX )
and action_94 lexbuf = (
 DECINT (getLexeme lexbuf) )
and action_93 lexbuf = (
 dprint ("IntegerLiteral: got HEXINT "^(getLexeme lexbuf)^"\n");
                                    HEXINT (getLexeme lexbuf) )
and action_92 lexbuf = (
 dprint ("IntegerLiteral: got DECINT "^(getLexeme lexbuf)^"\n");
                                    DECINT (getLexeme lexbuf) )
and action_91 lexbuf = (
 dprint ("IntegerLiteral: got OCTINT "^(getLexeme lexbuf)^"\n");
                                    OCTINT (getLexeme lexbuf) )
and action_90 lexbuf = (
 lexError "expected an identifier" lexbuf )
and action_89 lexbuf = (
 lexingMode := HASH_DEFINITION_LWSlm;
                                                     IDENTIFIER (getLexeme lexbuf) )
and action_88 lexbuf = (
 let val s' = getLexeme lexbuf (*'*)
                                          val s = String.substring (s',0,(String.size s') - 1)
                                      in lexingMode := HASH_PARAMETER_DEFINITIONlm;
                                         PMIDENTIFIER s
                                      end )
and action_87 lexbuf = (
 HashDefine lexbuf )
and action_86 lexbuf = (
 lexError "expected an identifier, or ')'" lexbuf )
and action_85 lexbuf = (
 IDENTIFIER (getLexeme lexbuf) )
and action_84 lexbuf = (
 IDENTIFIER (getLexeme lexbuf) )
and action_83 lexbuf = (
 COMMA )
and action_82 lexbuf = (
 lexingMode := HASH_DEFINITION_LWSlm;
                                  RPAR )
and action_81 lexbuf = (
 HashParameterDefinition lexbuf )
and action_80 lexbuf = (
 lexingMode := SOLlm;
               EOL )
and action_79 lexbuf = (
 lexingMode := HASH_DEFINITIONlm;
                Token lexbuf )
and action_78 lexbuf = (
 HashDefinitionLWS lexbuf )
and action_77 lexbuf = (
 lexingMode := SOLlm;
               EOL )
and action_76 lexbuf = (
 lexError "ill-formed preprocessor definition" lexbuf )
and action_75 lexbuf = (
 HashDefinition lexbuf )
and action_74 lexbuf = (
 TEXT_FRAG (getLexeme lexbuf) )
and action_73 lexbuf = (
 TOKEN_PASTE )
and action_72 lexbuf = (
 IDENTIFIER (getLexeme lexbuf) )
and action_71 lexbuf = (
 lexError "unmatched comment bracket" lexbuf )
and action_70 lexbuf = (
 savedLexemeStart := getLexemeStart lexbuf;
               comment_depth := 1; CommentNoNewLine lexbuf
             )
and action_69 lexbuf = (
 lexingMode := SOLlm;
               EOL )
and action_68 lexbuf = (
 Tokens lexbuf )
and action_67 lexbuf = (
 lexError "unmatched comment bracket" lexbuf )
and action_66 lexbuf = (
 savedLexemeStart := getLexemeStart lexbuf;
                comment_depth := 1; CommentNoNewLine lexbuf
              )
and action_65 lexbuf = (
 lexingMode := SOLlm;
                            EOL )
and action_64 lexbuf = (
 CPPLine lexbuf )
and action_63 lexbuf = (
 lexingMode := SOLlm;
                EOL )
and action_62 lexbuf = (
 CPPLine lexbuf )
and action_61 lexbuf = (
 lexError "unexpected character on #include line" lexbuf )
and action_60 lexbuf = (
 dprint ("#include: got \"<\"\n");
        scanString SysInclude lexbuf;
        SYSINCLUDE (get_stored_string())
      )
and action_59 lexbuf = (
 scanString String lexbuf;
        INCLUDE (get_stored_string())
      )
and action_58 lexbuf = (
 lexError "unmatched comment bracket" lexbuf )
and action_57 lexbuf = (
 savedLexemeStart := getLexemeStart lexbuf;
                comment_depth := 1; CommentNoNewLine lexbuf
              )
and action_56 lexbuf = (
 lexingMode := SOLlm;
                            EOL )
and action_55 lexbuf = (
 CPPInclude lexbuf )
and action_54 lexbuf = (
 lexingMode := SOLlm;
                EOL )
and action_53 lexbuf = (
 CPPInclude lexbuf )
and action_52 lexbuf = (
 CommentNoNewLine lexbuf )
and action_51 lexbuf = (
 notTerminated "comment" lexbuf )
and action_50 lexbuf = (
 lexingMode := SOLlm;
             EOL )
and action_49 lexbuf = (
 (decr comment_depth;
         if !comment_depth > 0 then CommentNoNewLine lexbuf else Token lexbuf) )
and action_48 lexbuf = (
 (incr comment_depth; CommentNoNewLine lexbuf) )
and action_47 lexbuf = (
 Comment lexbuf )
and action_46 lexbuf = (
 notTerminated "comment" lexbuf )
and action_45 lexbuf = (
 (decr comment_depth;
         if !comment_depth > 0 then Comment lexbuf else ()) )
and action_44 lexbuf = (
 (incr comment_depth; Comment lexbuf) )
and action_43 lexbuf = (
 lexError "ill-formed UTF8 character code" lexbuf )
and action_42 lexbuf = (
 store_string_char(getLexemeChar lexbuf 0);
                                                            store_string_char(getLexemeChar lexbuf 1);
                                                            store_string_char(getLexemeChar lexbuf 2);
                                                            store_string_char(getLexemeChar lexbuf 3) )
and action_41 lexbuf = (
 store_string_char(getLexemeChar lexbuf 0);
                                                            store_string_char(getLexemeChar lexbuf 1);
                                                            store_string_char(getLexemeChar lexbuf 2);
                                                            store_string_char(getLexemeChar lexbuf 3) )
and action_40 lexbuf = (
 store_string_char(getLexemeChar lexbuf 0);
                                                            store_string_char(getLexemeChar lexbuf 1);
                                                            store_string_char(getLexemeChar lexbuf 2);
                                                            store_string_char(getLexemeChar lexbuf 3) )
and action_39 lexbuf = (
 store_string_char(getLexemeChar lexbuf 0);
                                                    store_string_char(getLexemeChar lexbuf 1);
                                                    store_string_char(getLexemeChar lexbuf 2) )
and action_38 lexbuf = (
 store_string_char(getLexemeChar lexbuf 0);
                                            store_string_char(getLexemeChar lexbuf 1);
                                            store_string_char(getLexemeChar lexbuf 2) )
and action_37 lexbuf = (
 store_string_char(getLexemeChar lexbuf 0);
                                                     store_string_char(getLexemeChar lexbuf 1);
                                                     store_string_char(getLexemeChar lexbuf 2) )
and action_36 lexbuf = (
 store_string_char(getLexemeChar lexbuf 0);
                                            store_string_char(getLexemeChar lexbuf 1);
                                            store_string_char(getLexemeChar lexbuf 2) )
and action_35 lexbuf = (
 store_string_char(getLexemeChar lexbuf 0);
                                     store_string_char(getLexemeChar lexbuf 1) )
and action_34 lexbuf = (
 let val c = getLexemeChar lexbuf 0
                    in if c = #">" then dprint "UTF8Char: got \">\"\n" else ();
                       store_string_char c
                    end )
and action_33 lexbuf = (
 UTF8Char lexbuf;
         String lexbuf )
and action_32 lexbuf = (
 skipString "newline not permitted in string" SkipString lexbuf )
and action_31 lexbuf = (
 notTerminated "string" lexbuf )
and action_30 lexbuf = (
 skipString "ill-formed string escape sequence" SkipString lexbuf )
and action_29 lexbuf = (
 UTF8StringOfUCSEscapeSequence lexbuf 1
        handle BadUTF8 s => skipString s SkipString lexbuf;
        String lexbuf )
and action_28 lexbuf = (
 let val code = intOfString 16 2 (getLexeme lexbuf) in
          store_string_char(Char.chr code);
          String lexbuf
        end )
and action_27 lexbuf = (
 let val code = intOfString 8 1 (getLexeme lexbuf) in
          store_string_char(Char.chr code);
          String lexbuf
        end )
and action_26 lexbuf = (
 store_string_char(char_for_backslash(getLexemeChar lexbuf 1));
        String lexbuf )
and action_25 lexbuf = (
 () )
and action_24 lexbuf = (
 UTF8Char lexbuf;
         SysInclude lexbuf )
and action_23 lexbuf = (
 skipString "newline not permitted in string" SkipString lexbuf )
and action_22 lexbuf = (
 notTerminated "string" lexbuf )
and action_21 lexbuf = (
 skipString "ill-formed string escape sequence" SkipString lexbuf )
and action_20 lexbuf = (
 UTF8StringOfUCSEscapeSequence lexbuf 1
        handle BadUTF8 s => skipString s SkipString lexbuf;
        SysInclude lexbuf )
and action_19 lexbuf = (
 let val code = intOfString 16 2 (getLexeme lexbuf) in
          store_string_char(Char.chr code);
          SysInclude lexbuf
        end )
and action_18 lexbuf = (
 let val code = intOfString 8 1 (getLexeme lexbuf) in
          store_string_char(Char.chr code);
          SysInclude lexbuf
        end )
and action_17 lexbuf = (
 let val c = getLexemeChar lexbuf 1
        in if c = #">" then store_string_char c 
                       else store_string_char(char_for_backslash c);
           SysInclude lexbuf
        end )
and action_16 lexbuf = (
 () )
and action_15 lexbuf = (
 UTF8Char lexbuf;
         Char lexbuf )
and action_14 lexbuf = (
 skipString "newline not permitted in character" SkipChar lexbuf )
and action_13 lexbuf = (
 notTerminated "character" lexbuf )
and action_12 lexbuf = (
 skipString "ill-formed character escape sequence" SkipChar lexbuf )
and action_11 lexbuf = (
 let val code = intOfString 16 2 (getLexeme lexbuf) in
          store_string_char(Char.chr code);
          Char lexbuf
        end )
and action_10 lexbuf = (
 let val code = intOfString 8 1 (getLexeme lexbuf) in
          store_string_char(Char.chr code);
          Char lexbuf
        end )
and action_9 lexbuf = (
 store_string_char(char_for_backslash(getLexemeChar lexbuf 1));
        Char lexbuf )
and action_8 lexbuf = (
 () )
and action_7 lexbuf = (
 SkipString lexbuf )
and action_6 lexbuf = (
 notTerminated "string" lexbuf )
and action_5 lexbuf = (
 SkipString lexbuf )
and action_4 lexbuf = (
 () )
and action_3 lexbuf = (
 SkipChar lexbuf )
and action_2 lexbuf = (
 notTerminated "character" lexbuf )
and action_1 lexbuf = (
 SkipChar lexbuf )
and action_0 lexbuf = (
 () )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\\" => state_304 lexbuf
 |  #"'" => action_0 lexbuf
 |  #"\^Z" => action_2 lexbuf
 |  #"\^@" => action_2 lexbuf
 |  _ => action_3 lexbuf
 end)
and state_1 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\\" => state_298 lexbuf
 |  #"\"" => action_4 lexbuf
 |  #"\^Z" => action_6 lexbuf
 |  #"\^@" => action_6 lexbuf
 |  _ => action_7 lexbuf
 end)
and state_2 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_15);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_14 lexbuf
 |  #"\r" => action_14 lexbuf
 |  #"\^@" => action_13 lexbuf
 |  #"\^Z" => action_13 lexbuf
 |  #"\\" => state_286 lexbuf
 |  #"'" => action_8 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_3 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_24);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_23 lexbuf
 |  #"\r" => action_23 lexbuf
 |  #"\^@" => action_22 lexbuf
 |  #"\^Z" => action_22 lexbuf
 |  #"\\" => state_264 lexbuf
 |  #">" => action_16 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_4 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_33);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_32 lexbuf
 |  #"\r" => action_32 lexbuf
 |  #"\^@" => action_31 lexbuf
 |  #"\^Z" => action_31 lexbuf
 |  #"\\" => state_242 lexbuf
 |  #"\"" => action_25 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_5 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\193" then  action_43 lexbuf
 else if currChar >= #"\245" andalso currChar <= #"\255" then  action_43 lexbuf
 else if currChar >= #"\194" andalso currChar <= #"\223" then  state_213 lexbuf
 else if currChar >= #"\225" andalso currChar <= #"\236" then  state_215 lexbuf
 else case currChar of
    #"\243" => state_219 lexbuf
 |  #"\242" => state_219 lexbuf
 |  #"\241" => state_219 lexbuf
 |  #"\239" => state_217 lexbuf
 |  #"\238" => state_217 lexbuf
 |  #"\244" => state_220 lexbuf
 |  #"\240" => state_218 lexbuf
 |  #"\237" => state_216 lexbuf
 |  #"\224" => state_214 lexbuf
 |  #"\^@" => action_34 lexbuf
 |  _ => action_34 lexbuf
 end)
and state_6 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => state_207 lexbuf
 |  #"*" => state_206 lexbuf
 |  #"\^Z" => action_46 lexbuf
 |  #"\^@" => action_46 lexbuf
 |  _ => action_47 lexbuf
 end)
and state_7 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => state_200 lexbuf
 |  #"*" => state_199 lexbuf
 |  #"\^Z" => action_51 lexbuf
 |  #"\n" => action_50 lexbuf
 |  #"\^@" => action_51 lexbuf
 |  _ => action_52 lexbuf
 end)
and state_8 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => action_53 lexbuf
 |  #" " => action_53 lexbuf
 |  #"\\" => state_189 lexbuf
 |  #"<" => action_60 lexbuf
 |  #"/" => state_187 lexbuf
 |  #"*" => state_186 lexbuf
 |  #"\"" => action_59 lexbuf
 |  #"\n" => action_54 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => action_61 lexbuf
 end)
and state_9 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_68);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => action_62 lexbuf
 |  #" " => action_62 lexbuf
 |  #"\\" => state_176 lexbuf
 |  #"/" => state_175 lexbuf
 |  #"*" => state_174 lexbuf
 |  #"\n" => action_63 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_10 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_164 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_164 lexbuf
 else case currChar of
    #"_" => state_164 lexbuf
 |  #"\\" => state_165 lexbuf
 |  #"/" => state_163 lexbuf
 |  #"*" => state_162 lexbuf
 |  #"#" => state_161 lexbuf
 |  #"\n" => action_69 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_159 lexbuf
 end)
and state_11 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_79);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => action_78 lexbuf
 |  #" " => action_78 lexbuf
 |  #"\n" => action_77 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_12 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_153 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_153 lexbuf
 else case currChar of
    #"_" => state_153 lexbuf
 |  #"\t" => action_81 lexbuf
 |  #" " => action_81 lexbuf
 |  #"." => state_152 lexbuf
 |  #"," => action_83 lexbuf
 |  #")" => action_82 lexbuf
 |  #"\n" => action_80 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => action_86 lexbuf
 end)
and state_13 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_144 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_144 lexbuf
 else case currChar of
    #"_" => state_144 lexbuf
 |  #"\t" => action_87 lexbuf
 |  #" " => action_87 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => action_90 lexbuf
 end)
and state_14 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_97);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"1" andalso currChar <= #"9" then  state_136 lexbuf
 else case currChar of
    #"U" => action_95 lexbuf
 |  #"u" => action_95 lexbuf
 |  #"L" => action_96 lexbuf
 |  #"l" => action_96 lexbuf
 |  #"0" => state_135 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_15 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_103);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_132 lexbuf
 else case currChar of
    #"F" => action_102 lexbuf
 |  #"L" => action_102 lexbuf
 |  #"f" => action_102 lexbuf
 |  #"l" => action_102 lexbuf
 |  #"E" => action_99 lexbuf
 |  #"e" => action_99 lexbuf
 |  #"+" => action_101 lexbuf
 |  #"-" => action_101 lexbuf
 |  #"." => action_100 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_16 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"K" then  state_114 lexbuf
 else if currChar >= #"M" andalso currChar <= #"Z" then  state_114 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_114 lexbuf
 else if currChar >= #"1" andalso currChar <= #"9" then  state_110 lexbuf
 else case currChar of
    #"_" => state_114 lexbuf
 |  #"!" => state_101 lexbuf
 |  #"&" => state_101 lexbuf
 |  #"%" => state_101 lexbuf
 |  #"+" => state_101 lexbuf
 |  #"/" => state_101 lexbuf
 |  #"." => state_101 lexbuf
 |  #"-" => state_101 lexbuf
 |  #">" => state_101 lexbuf
 |  #"=" => state_101 lexbuf
 |  #"<" => state_101 lexbuf
 |  #"^" => state_101 lexbuf
 |  #"|" => state_101 lexbuf
 |  #"~" => action_108 lexbuf
 |  #"}" => action_113 lexbuf
 |  #"{" => action_112 lexbuf
 |  #"]" => action_115 lexbuf
 |  #"[" => action_114 lexbuf
 |  #"L" => state_115 lexbuf
 |  #"?" => state_113 lexbuf
 |  #";" => action_116 lexbuf
 |  #":" => action_118 lexbuf
 |  #"0" => state_109 lexbuf
 |  #"," => action_119 lexbuf
 |  #"*" => state_107 lexbuf
 |  #")" => action_111 lexbuf
 |  #"(" => action_110 lexbuf
 |  #"'" => action_123 lexbuf
 |  #"$" => action_109 lexbuf
 |  #"\"" => action_122 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => action_124 lexbuf
 end)
and state_17 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_131);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\r" => action_126 lexbuf
 |  #"\f" => action_126 lexbuf
 |  #"\t" => state_89 lexbuf
 |  #" " => state_89 lexbuf
 |  #"\^@" => action_130 lexbuf
 |  #"\^Z" => action_130 lexbuf
 |  #"/" => state_93 lexbuf
 |  #"*" => state_92 lexbuf
 |  #"\n" => action_125 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_18 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_146);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_132 lexbuf
 |  #"\t" => action_132 lexbuf
 |  #"\r" => action_132 lexbuf
 |  #"\f" => action_132 lexbuf
 |  #" " => action_132 lexbuf
 |  #"#" => state_21 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_19 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_148);
 let val currChar = getNextChar lexbuf in
 case currChar of
    _ => backtrack lexbuf
 end)
and state_21 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_22 lexbuf
 |  #" " => state_22 lexbuf
 |  #"w" => state_28 lexbuf
 |  #"u" => state_27 lexbuf
 |  #"p" => state_26 lexbuf
 |  #"i" => state_25 lexbuf
 |  #"e" => state_24 lexbuf
 |  #"d" => state_23 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_22 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_86 lexbuf
 else case currChar of
    #"\t" => state_22 lexbuf
 |  #" " => state_22 lexbuf
 |  #"w" => state_28 lexbuf
 |  #"u" => state_27 lexbuf
 |  #"p" => state_26 lexbuf
 |  #"i" => state_25 lexbuf
 |  #"e" => state_24 lexbuf
 |  #"d" => state_23 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_23 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_80 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_24 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_67 lexbuf
 |  #"n" => state_66 lexbuf
 |  #"l" => state_65 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_25 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_48 lexbuf
 |  #"f" => state_47 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_26 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_41 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_27 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_36 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_28 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_29 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_29 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_30 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_30 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_31 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_32 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_32 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_33 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_33 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"g" => state_34 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_34 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_35 lexbuf
 |  #" " => state_35 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_35 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_139);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_35 lexbuf
 |  #" " => state_35 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_36 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_37 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_37 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_38 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_38 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"f" => state_39 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_39 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_40 lexbuf
 |  #" " => state_40 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_40 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_143);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_40 lexbuf
 |  #" " => state_40 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_41 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_42 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_42 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"g" => state_43 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_43 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"m" => state_44 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_44 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"a" => state_45 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_45 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_46 lexbuf
 |  #" " => state_46 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_46 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_140);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_46 lexbuf
 |  #" " => state_46 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_47 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_55 lexbuf
 |  #" " => state_55 lexbuf
 |  #"n" => state_57 lexbuf
 |  #"d" => state_56 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_48 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => state_49 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_49 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"l" => state_50 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_50 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"u" => state_51 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_51 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_52 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_52 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_53 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_53 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_54 lexbuf
 |  #" " => state_54 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_54 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_135);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_54 lexbuf
 |  #" " => state_54 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_55 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_136);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_55 lexbuf
 |  #" " => state_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_56 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_62 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_57 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_58 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_58 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_59 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_59 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"f" => state_60 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_60 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_61 lexbuf
 |  #" " => state_61 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_61 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_142);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_61 lexbuf
 |  #" " => state_61 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_62 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"f" => state_63 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_63 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_64 lexbuf
 |  #" " => state_64 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_64 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_141);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_64 lexbuf
 |  #" " => state_64 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_65 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"s" => state_76 lexbuf
 |  #"i" => state_75 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_66 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"d" => state_72 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_67 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_68 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_68 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_69 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_69 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"r" => state_70 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_70 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_71 lexbuf
 |  #" " => state_71 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_71 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_138);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_71 lexbuf
 |  #" " => state_71 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_72 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_73 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_73 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"f" => action_145 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_75 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"f" => state_78 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_76 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => action_144 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_78 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_79 lexbuf
 |  #" " => state_79 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_79 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_137);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_79 lexbuf
 |  #" " => state_79 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_80 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"f" => state_81 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_81 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"i" => state_82 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_82 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"n" => state_83 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_83 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"e" => state_84 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_84 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_85 lexbuf
 |  #" " => state_85 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_85 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_134);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_85 lexbuf
 |  #" " => state_85 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_86 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_133);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_86 lexbuf
 else case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_87 lexbuf
 end)
and state_87 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_133);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_87 lexbuf
 end)
and state_89 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_126);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_98 lexbuf
 |  #" " => state_98 lexbuf
 |  #"/" => state_99 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_92 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => action_129 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_93 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => state_95 lexbuf
 |  #"*" => action_128 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_95 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_127 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_95 lexbuf
 end)
and state_98 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_98 lexbuf
 |  #" " => state_98 lexbuf
 |  #"/" => state_99 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_99 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => state_95 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_101 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_105);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"!" => state_124 lexbuf
 |  #"&" => state_124 lexbuf
 |  #"%" => state_124 lexbuf
 |  #"+" => state_124 lexbuf
 |  #"/" => state_124 lexbuf
 |  #"." => state_124 lexbuf
 |  #"-" => state_124 lexbuf
 |  #"?" => state_124 lexbuf
 |  #">" => state_124 lexbuf
 |  #"=" => state_124 lexbuf
 |  #"<" => state_124 lexbuf
 |  #"^" => state_124 lexbuf
 |  #"|" => state_124 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_107 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_107);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_106 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_109 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_121);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_126 lexbuf
 else case currChar of
    #"." => action_120 lexbuf
 |  #"E" => action_120 lexbuf
 |  #"e" => action_120 lexbuf
 |  #"X" => state_127 lexbuf
 |  #"x" => state_127 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_110 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_121);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_126 lexbuf
 else case currChar of
    #"." => action_120 lexbuf
 |  #"E" => action_120 lexbuf
 |  #"e" => action_120 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_113 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_105);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"!" => state_124 lexbuf
 |  #"&" => state_124 lexbuf
 |  #"%" => state_124 lexbuf
 |  #"+" => state_124 lexbuf
 |  #"/" => state_124 lexbuf
 |  #"." => state_124 lexbuf
 |  #"-" => state_124 lexbuf
 |  #"?" => state_124 lexbuf
 |  #">" => state_124 lexbuf
 |  #"=" => state_124 lexbuf
 |  #"<" => state_124 lexbuf
 |  #"^" => state_124 lexbuf
 |  #"|" => state_124 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_114 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_104);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_123 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_123 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_123 lexbuf
 else case currChar of
    #"_" => state_123 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_115 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_104);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_123 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_123 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_123 lexbuf
 else case currChar of
    #"_" => state_123 lexbuf
 |  #"'" => action_123 lexbuf
 |  #"\"" => action_122 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_123 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_104);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_123 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_123 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_123 lexbuf
 else case currChar of
    #"_" => state_123 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_124 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_105);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"!" => state_124 lexbuf
 |  #"&" => state_124 lexbuf
 |  #"%" => state_124 lexbuf
 |  #"+" => state_124 lexbuf
 |  #"/" => state_124 lexbuf
 |  #"." => state_124 lexbuf
 |  #"-" => state_124 lexbuf
 |  #"?" => state_124 lexbuf
 |  #">" => state_124 lexbuf
 |  #"=" => state_124 lexbuf
 |  #"<" => state_124 lexbuf
 |  #"^" => state_124 lexbuf
 |  #"|" => state_124 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_126 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_121);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_126 lexbuf
 else case currChar of
    #"." => action_120 lexbuf
 |  #"E" => action_120 lexbuf
 |  #"e" => action_120 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_127 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_128 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_128 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_128 lexbuf
 else backtrack lexbuf
 end)
and state_128 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_121);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_128 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_128 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_128 lexbuf
 else backtrack lexbuf
 end)
and state_132 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_98);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_132 lexbuf
 else backtrack lexbuf
 end)
and state_135 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_94);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"7" then  state_139 lexbuf
 else case currChar of
    #"X" => state_140 lexbuf
 |  #"x" => state_140 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_136 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_92);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_136 lexbuf
 else backtrack lexbuf
 end)
and state_139 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_91);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"7" then  state_139 lexbuf
 else backtrack lexbuf
 end)
and state_140 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_141 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_141 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_141 lexbuf
 else backtrack lexbuf
 end)
and state_141 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_93);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_141 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_141 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_141 lexbuf
 else backtrack lexbuf
 end)
and state_144 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_89);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_146 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_146 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_146 lexbuf
 else case currChar of
    #"_" => state_146 lexbuf
 |  #"(" => action_88 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_146 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_89);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_146 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_146 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_146 lexbuf
 else case currChar of
    #"_" => state_146 lexbuf
 |  #"(" => action_88 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_152 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_86);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"." => state_155 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_153 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_84);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_154 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_154 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_154 lexbuf
 else case currChar of
    #"_" => state_154 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_154 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_84);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_154 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_154 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_154 lexbuf
 else case currChar of
    #"_" => state_154 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_155 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"." => action_85 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_159 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_74);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_166 lexbuf
 end)
and state_161 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_74);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  #"#" => state_171 lexbuf
 |  _ => state_166 lexbuf
 end)
and state_162 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_74);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  #"/" => state_170 lexbuf
 |  _ => state_166 lexbuf
 end)
and state_163 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_74);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  #"*" => state_169 lexbuf
 |  _ => state_166 lexbuf
 end)
and state_164 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_72);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_168 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_168 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_168 lexbuf
 else case currChar of
    #"_" => state_168 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_166 lexbuf
 end)
and state_165 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_74);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_75 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_166 lexbuf
 end)
and state_166 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_74);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_166 lexbuf
 end)
and state_168 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_72);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_168 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_168 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_168 lexbuf
 else case currChar of
    #"_" => state_168 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_166 lexbuf
 end)
and state_169 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_70);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_166 lexbuf
 end)
and state_170 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_71);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_166 lexbuf
 end)
and state_171 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_73);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_166 lexbuf
 end)
and state_174 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => action_67 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_175 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => state_179 lexbuf
 |  #"*" => action_66 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_176 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_64 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_179 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_65 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_179 lexbuf
 end)
and state_186 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_61);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => action_58 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_187 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_61);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => state_192 lexbuf
 |  #"*" => action_57 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_189 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_61);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_192 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_56 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_192 lexbuf
 end)
and state_199 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_52);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => action_49 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_200 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_52);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => action_48 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_206 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_47);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => action_45 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_207 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_47);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => action_44 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_213 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  action_35 lexbuf
 else backtrack lexbuf
 end)
and state_214 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\160" andalso currChar <= #"\191" then  state_236 lexbuf
 else backtrack lexbuf
 end)
and state_215 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  state_234 lexbuf
 else backtrack lexbuf
 end)
and state_216 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\159" then  state_232 lexbuf
 else backtrack lexbuf
 end)
and state_217 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  state_230 lexbuf
 else backtrack lexbuf
 end)
and state_218 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\144" andalso currChar <= #"\191" then  state_227 lexbuf
 else backtrack lexbuf
 end)
and state_219 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  state_224 lexbuf
 else backtrack lexbuf
 end)
and state_220 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_43);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\143" then  state_221 lexbuf
 else backtrack lexbuf
 end)
and state_221 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  state_222 lexbuf
 else backtrack lexbuf
 end)
and state_222 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  action_42 lexbuf
 else backtrack lexbuf
 end)
and state_224 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  state_225 lexbuf
 else backtrack lexbuf
 end)
and state_225 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  action_41 lexbuf
 else backtrack lexbuf
 end)
and state_227 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  state_228 lexbuf
 else backtrack lexbuf
 end)
and state_228 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  action_40 lexbuf
 else backtrack lexbuf
 end)
and state_230 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  action_39 lexbuf
 else backtrack lexbuf
 end)
and state_232 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  action_38 lexbuf
 else backtrack lexbuf
 end)
and state_234 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  action_37 lexbuf
 else backtrack lexbuf
 end)
and state_236 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\128" andalso currChar <= #"\191" then  action_36 lexbuf
 else backtrack lexbuf
 end)
and state_242 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_30);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"F" then  state_247 lexbuf
 else case currChar of
    #"9" => state_247 lexbuf
 |  #"8" => state_247 lexbuf
 |  #"e" => state_247 lexbuf
 |  #"d" => state_247 lexbuf
 |  #"c" => state_247 lexbuf
 |  #"\"" => action_26 lexbuf
 |  #"'" => action_26 lexbuf
 |  #"?" => action_26 lexbuf
 |  #"\\" => action_26 lexbuf
 |  #"n" => action_26 lexbuf
 |  #"r" => action_26 lexbuf
 |  #"t" => action_26 lexbuf
 |  #"v" => action_26 lexbuf
 |  #"7" => state_246 lexbuf
 |  #"6" => state_246 lexbuf
 |  #"5" => state_246 lexbuf
 |  #"4" => state_246 lexbuf
 |  #"3" => state_245 lexbuf
 |  #"2" => state_245 lexbuf
 |  #"1" => state_245 lexbuf
 |  #"0" => state_245 lexbuf
 |  #"b" => state_250 lexbuf
 |  #"a" => state_250 lexbuf
 |  #"f" => state_250 lexbuf
 |  #"X" => state_249 lexbuf
 |  #"x" => state_249 lexbuf
 |  #"U" => state_248 lexbuf
 |  #"u" => state_248 lexbuf
 |  #"+" => state_244 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_244 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_247 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_247 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_247 lexbuf
 else backtrack lexbuf
 end)
and state_245 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"F" then  state_251 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_251 lexbuf
 else if currChar >= #"0" andalso currChar <= #"7" then  state_259 lexbuf
 else case currChar of
    #"9" => state_251 lexbuf
 |  #"8" => state_251 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_246 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"F" then  state_251 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_251 lexbuf
 else if currChar >= #"0" andalso currChar <= #"7" then  state_258 lexbuf
 else case currChar of
    #"9" => state_251 lexbuf
 |  #"8" => state_251 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_247 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_251 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_251 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_251 lexbuf
 else backtrack lexbuf
 end)
and state_248 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_247 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_247 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_247 lexbuf
 else case currChar of
    #"+" => state_244 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_249 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_256 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_256 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_256 lexbuf
 else backtrack lexbuf
 end)
and state_250 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_26);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_251 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_251 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_251 lexbuf
 else backtrack lexbuf
 end)
and state_251 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_252 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_252 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_252 lexbuf
 else backtrack lexbuf
 end)
and state_252 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_253 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_253 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_253 lexbuf
 else backtrack lexbuf
 end)
and state_253 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_29);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_254 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_254 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_254 lexbuf
 else backtrack lexbuf
 end)
and state_254 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_29);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  action_29 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  action_29 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  action_29 lexbuf
 else backtrack lexbuf
 end)
and state_256 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_28);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  action_28 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  action_28 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  action_28 lexbuf
 else backtrack lexbuf
 end)
and state_258 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_252 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_252 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_252 lexbuf
 else backtrack lexbuf
 end)
and state_259 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"F" then  state_252 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_252 lexbuf
 else if currChar >= #"0" andalso currChar <= #"7" then  state_260 lexbuf
 else case currChar of
    #"9" => state_252 lexbuf
 |  #"8" => state_252 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_260 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_27);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_253 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_253 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_253 lexbuf
 else backtrack lexbuf
 end)
and state_264 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_21);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"F" then  state_269 lexbuf
 else case currChar of
    #"9" => state_269 lexbuf
 |  #"8" => state_269 lexbuf
 |  #"e" => state_269 lexbuf
 |  #"d" => state_269 lexbuf
 |  #"c" => state_269 lexbuf
 |  #"\"" => action_17 lexbuf
 |  #"'" => action_17 lexbuf
 |  #"?" => action_17 lexbuf
 |  #">" => action_17 lexbuf
 |  #"\\" => action_17 lexbuf
 |  #"n" => action_17 lexbuf
 |  #"r" => action_17 lexbuf
 |  #"t" => action_17 lexbuf
 |  #"v" => action_17 lexbuf
 |  #"7" => state_268 lexbuf
 |  #"6" => state_268 lexbuf
 |  #"5" => state_268 lexbuf
 |  #"4" => state_268 lexbuf
 |  #"3" => state_267 lexbuf
 |  #"2" => state_267 lexbuf
 |  #"1" => state_267 lexbuf
 |  #"0" => state_267 lexbuf
 |  #"b" => state_272 lexbuf
 |  #"a" => state_272 lexbuf
 |  #"f" => state_272 lexbuf
 |  #"X" => state_271 lexbuf
 |  #"x" => state_271 lexbuf
 |  #"U" => state_270 lexbuf
 |  #"u" => state_270 lexbuf
 |  #"+" => state_266 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_266 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_269 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_269 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_269 lexbuf
 else backtrack lexbuf
 end)
and state_267 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_18);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"F" then  state_273 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_273 lexbuf
 else if currChar >= #"0" andalso currChar <= #"7" then  state_281 lexbuf
 else case currChar of
    #"9" => state_273 lexbuf
 |  #"8" => state_273 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_268 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_18);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"F" then  state_273 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_273 lexbuf
 else if currChar >= #"0" andalso currChar <= #"7" then  state_280 lexbuf
 else case currChar of
    #"9" => state_273 lexbuf
 |  #"8" => state_273 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_269 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_273 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_273 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_273 lexbuf
 else backtrack lexbuf
 end)
and state_270 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_269 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_269 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_269 lexbuf
 else case currChar of
    #"+" => state_266 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_271 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_278 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_278 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_278 lexbuf
 else backtrack lexbuf
 end)
and state_272 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_17);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_273 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_273 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_273 lexbuf
 else backtrack lexbuf
 end)
and state_273 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_274 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_274 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_274 lexbuf
 else backtrack lexbuf
 end)
and state_274 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_275 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_275 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_275 lexbuf
 else backtrack lexbuf
 end)
and state_275 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_20);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_276 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_276 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_276 lexbuf
 else backtrack lexbuf
 end)
and state_276 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_20);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  action_20 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  action_20 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  action_20 lexbuf
 else backtrack lexbuf
 end)
and state_278 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_19);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  action_19 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  action_19 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  action_19 lexbuf
 else backtrack lexbuf
 end)
and state_280 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_18);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_274 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_274 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_274 lexbuf
 else backtrack lexbuf
 end)
and state_281 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_18);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"F" then  state_274 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_274 lexbuf
 else if currChar >= #"0" andalso currChar <= #"7" then  state_282 lexbuf
 else case currChar of
    #"9" => state_274 lexbuf
 |  #"8" => state_274 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_282 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_18);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_275 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_275 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_275 lexbuf
 else backtrack lexbuf
 end)
and state_286 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_12);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\"" => action_9 lexbuf
 |  #"'" => action_9 lexbuf
 |  #"?" => action_9 lexbuf
 |  #"\\" => action_9 lexbuf
 |  #"b" => action_9 lexbuf
 |  #"a" => action_9 lexbuf
 |  #"f" => action_9 lexbuf
 |  #"n" => action_9 lexbuf
 |  #"r" => action_9 lexbuf
 |  #"t" => action_9 lexbuf
 |  #"v" => action_9 lexbuf
 |  #"7" => state_289 lexbuf
 |  #"6" => state_289 lexbuf
 |  #"5" => state_289 lexbuf
 |  #"4" => state_289 lexbuf
 |  #"3" => state_288 lexbuf
 |  #"2" => state_288 lexbuf
 |  #"1" => state_288 lexbuf
 |  #"0" => state_288 lexbuf
 |  #"X" => state_290 lexbuf
 |  #"x" => state_290 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_288 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"7" then  state_289 lexbuf
 else backtrack lexbuf
 end)
and state_289 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"7" then  action_10 lexbuf
 else backtrack lexbuf
 end)
and state_290 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_291 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_291 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_291 lexbuf
 else backtrack lexbuf
 end)
and state_291 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_11);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  action_11 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  action_11 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  action_11 lexbuf
 else backtrack lexbuf
 end)
and state_298 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_7);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\"" => action_5 lexbuf
 |  #"\\" => action_5 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_304 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_3);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"'" => action_1 lexbuf
 |  #"\\" => action_1 lexbuf
 |  _ => backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_19 lexbuf)

and TokenSOL lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_18 lexbuf)

and TokenN lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_17 lexbuf)

and Tokens lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_16 lexbuf)

and FloatLiteral lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_15 lexbuf)

and IntegerLiteral lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_14 lexbuf)

and HashDefine lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_13 lexbuf)

and HashParameterDefinition lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_12 lexbuf)

and HashDefinitionLWS lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_11 lexbuf)

and HashDefinition lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_10 lexbuf)

and CPPLine lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_9 lexbuf)

and CPPInclude lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_8 lexbuf)

and CommentNoNewLine lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_7 lexbuf)

and Comment lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_6 lexbuf)

and UTF8Char lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_5 lexbuf)

and String lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_4 lexbuf)

and SysInclude lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_3 lexbuf)

and Char lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_2 lexbuf)

and SkipString lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_1 lexbuf)

and SkipChar lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_148, action_147];
val _ = fn _ => [action_146, action_145, action_144, action_143, action_142, action_141, action_140, action_139, action_138, action_137, action_136, action_135, action_134, action_133, action_132];
val _ = fn _ => [action_131, action_130, action_129, action_128, action_127, action_126, action_125];
val _ = fn _ => [action_124, action_123, action_122, action_121, action_120, action_119, action_118, action_117, action_116, action_115, action_114, action_113, action_112, action_111, action_110, action_109, action_108, action_107, action_106, action_105, action_104];
val _ = fn _ => [action_103, action_102, action_101, action_100, action_99, action_98];
val _ = fn _ => [action_97, action_96, action_95, action_94, action_93, action_92, action_91];
val _ = fn _ => [action_90, action_89, action_88, action_87];
val _ = fn _ => [action_86, action_85, action_84, action_83, action_82, action_81, action_80];
val _ = fn _ => [action_79, action_78, action_77];
val _ = fn _ => [action_76, action_75, action_74, action_73, action_72, action_71, action_70, action_69];
val _ = fn _ => [action_68, action_67, action_66, action_65, action_64, action_63, action_62];
val _ = fn _ => [action_61, action_60, action_59, action_58, action_57, action_56, action_55, action_54, action_53];
val _ = fn _ => [action_52, action_51, action_50, action_49, action_48];
val _ = fn _ => [action_47, action_46, action_45, action_44];
val _ = fn _ => [action_43, action_42, action_41, action_40, action_39, action_38, action_37, action_36, action_35, action_34];
val _ = fn _ => [action_33, action_32, action_31, action_30, action_29, action_28, action_27, action_26, action_25];
val _ = fn _ => [action_24, action_23, action_22, action_21, action_20, action_19, action_18, action_17, action_16];
val _ = fn _ => [action_15, action_14, action_13, action_12, action_11, action_10, action_9, action_8];
val _ = fn _ => [action_7, action_6, action_5, action_4];
val _ = fn _ => [action_3, action_2, action_1, action_0];

end
