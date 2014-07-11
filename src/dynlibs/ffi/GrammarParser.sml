local
in
datatype token =
    BAR
  | COLON
  | EOF
  | EPSILON
  | EQUALS
  | IDENTIFIER of string
  | LITERAL of string
  | OF
  | ONE
  | OPT
  | RULE_IDENTIFIER of string
  | SEMICOLON
  | TERMINAL of string
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

open GrammarSyntax
(* Line 7, file GrammarParser.sml *)
val yytransl = #[
  257 (* BAR *),
  258 (* COLON *),
  259 (* EOF *),
  260 (* EPSILON *),
  261 (* EQUALS *),
  262 (* IDENTIFIER *),
  263 (* LITERAL *),
  264 (* OF *),
  265 (* ONE *),
  266 (* OPT *),
  267 (* RULE_IDENTIFIER *),
  268 (* SEMICOLON *),
  269 (* TERMINAL *),
    0];

val yylhs = "\255\255\
\\001\000\001\000\002\000\019\000\019\000\018\000\018\000\009\000\
\\009\000\010\000\010\000\011\000\011\000\016\000\016\000\015\000\
\\015\000\003\000\003\000\004\000\004\000\004\000\020\000\020\000\
\\005\000\005\000\006\000\006\000\007\000\008\000\008\000\012\000\
\\012\000\012\000\012\000\017\000\013\000\014\000\000\000";

val yylen = "\002\000\
\\002\000\001\000\004\000\001\000\002\000\001\000\001\000\002\000\
\\004\000\002\000\004\000\002\000\002\000\001\000\000\000\001\000\
\\002\000\001\000\002\000\002\000\004\000\004\000\001\000\002\000\
\\001\000\003\000\001\000\001\000\001\000\001\000\002\000\001\000\
\\002\000\001\000\002\000\001\000\001\000\001\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\002\000\000\000\039\000\000\000\006\000\007\000\
\\004\000\000\000\008\000\000\000\010\000\001\000\000\000\005\000\
\\000\000\000\000\014\000\016\000\000\000\000\000\000\000\000\000\
\\000\000\018\000\013\000\017\000\012\000\029\000\038\000\037\000\
\\000\000\000\000\025\000\028\000\000\000\030\000\000\000\000\000\
\\019\000\000\000\000\000\031\000\036\000\033\000\035\000\023\000\
\\000\000\000\000\026\000\024\000";

val yydgoto = "\002\000\
\\005\000\006\000\025\000\026\000\034\000\035\000\036\000\037\000\
\\007\000\008\000\021\000\038\000\039\000\040\000\022\000\023\000\
\\046\000\009\000\010\000\050\000";

val yysindex = "\003\000\
\\005\255\000\000\000\000\021\255\000\000\042\255\000\000\000\000\
\\000\000\013\255\000\000\038\255\000\000\000\000\035\255\000\000\
\\009\255\026\255\000\000\000\000\041\255\036\255\043\255\029\255\
\\026\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\044\255\050\255\000\000\000\000\014\255\000\000\045\255\045\255\
\\000\000\034\255\025\255\000\000\000\000\000\000\000\000\000\000\
\\041\255\047\255\000\000\000\000";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\049\255\000\000\000\000\000\000\030\255\032\255\000\000\000\000\
\\051\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\012\255\000\000\000\000\000\255\000\000\255\254\006\255\
\\000\000\049\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\015\255\016\255\000\000\000\000";

val yygindex = "\000\000\
\\000\000\000\000\000\000\032\000\000\000\015\000\000\000\000\000\
\\000\000\000\000\017\000\023\000\000\000\000\000\000\000\000\000\
\\021\000\052\000\000\000\000\000";

val YYTABLESIZE = 62;
val yytable = "\032\000\
\\027\000\032\000\027\000\001\000\032\000\032\000\034\000\003\000\
\\034\000\032\000\027\000\034\000\034\000\019\000\020\000\004\000\
\\034\000\021\000\022\000\031\000\032\000\020\000\020\000\004\000\
\\015\000\021\000\022\000\011\000\030\000\012\000\031\000\032\000\
\\030\000\013\000\031\000\032\000\024\000\033\000\019\000\048\000\
\\009\000\009\000\011\000\011\000\014\000\017\000\018\000\027\000\
\\028\000\029\000\043\000\042\000\052\000\003\000\045\000\015\000\
\\041\000\051\000\049\000\044\000\047\000\016\000";

val yycheck = "\001\001\
\\001\001\003\001\003\001\001\000\006\001\007\001\001\001\003\001\
\\003\001\011\001\011\001\006\001\007\001\005\001\003\001\011\001\
\\011\001\003\001\003\001\006\001\007\001\013\001\011\001\011\001\
\\012\001\011\001\011\001\007\001\004\001\009\001\006\001\007\001\
\\004\001\013\001\006\001\007\001\011\001\009\001\005\001\006\001\
\\011\001\012\001\011\001\012\001\003\001\008\001\012\001\007\001\
\\013\001\007\001\001\001\008\001\006\001\003\001\010\001\007\001\
\\025\000\043\000\042\000\037\000\040\000\010\000";

val yyact = vector_ 40 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file GrammarParser.grm, line 37 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
in
( (d__1__) ) end : GrammarSyntax.Tree))
;
(* Rule 2, file GrammarParser.grm, line 38 *)
val _ = update_ yyact 2
(fn () => repr(let
in
( mkNonTerm("rule-list",[]) ) end : GrammarSyntax.Tree))
;
(* Rule 3, file GrammarParser.grm, line 43 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 3 : GrammarSyntax.Tree
val d__4__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("rule-list",[(d__1__),(d__4__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 4, file GrammarParser.grm, line 47 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("lexical-rule-list", [(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 5, file GrammarParser.grm, line 48 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
val d__2__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("lexical-rule-list", [(d__1__),(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 6, file GrammarParser.grm, line 52 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("lexical-rule",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 7, file GrammarParser.grm, line 53 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("lexical-rule",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 8, file GrammarParser.grm, line 57 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 1 : string
val d__2__ = peekVal 0 : string
in
( mkNonTerm("literal-rule",[mkTerm((d__1__)),mkTerm((d__2__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 9, file GrammarParser.grm, line 58 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 3 : string
val d__4__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("literal-rule",[mkTerm((d__1__)),(d__4__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 10, file GrammarParser.grm, line 62 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 1 : string
val d__2__ = peekVal 0 : string
in
( mkNonTerm("terminal-rule",[mkTerm((d__1__)),mkTerm((d__2__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 11, file GrammarParser.grm, line 63 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 3 : string
val d__4__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("terminal-rule",[mkTerm((d__1__)),(d__4__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 12, file GrammarParser.grm, line 67 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
val d__2__ = peekVal 0 : string
in
( mkNonTerm("literal-list",[(d__1__),mkTerm((d__2__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 13, file GrammarParser.grm, line 68 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
val d__2__ = peekVal 0 : string
in
( mkNonTerm("literal-list",[(d__1__),mkTerm((d__2__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 14, file GrammarParser.grm, line 72 *)
val _ = update_ yyact 14
(fn () => repr(let
in
( mkNonTerm("optional-equals",[mkTerm("=")]) ) end : GrammarSyntax.Tree))
;
(* Rule 15, file GrammarParser.grm, line 73 *)
val _ = update_ yyact 15
(fn () => repr(let
in
( mkNonTerm("optional-equals",[]) ) end : GrammarSyntax.Tree))
;
(* Rule 16, file GrammarParser.grm, line 77 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
( mkNonTerm("terminal-list",[mkTerm((d__1__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 17, file GrammarParser.grm, line 78 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
val d__2__ = peekVal 0 : string
in
( mkNonTerm("terminal-list",[(d__1__),mkTerm((d__2__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 18, file GrammarParser.grm, line 82 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("grammatical-rule-list", [(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 19, file GrammarParser.grm, line 83 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
val d__2__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("grammatical-rule-list", [(d__1__),(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 20, file GrammarParser.grm, line 87 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 1 : string
val d__2__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("grammatical-rule",[mkTerm((d__1__)),(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 21, file GrammarParser.grm, line 88 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__1__ = peekVal 3 : string
val d__4__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("grammatical-rule",[mkTerm((d__1__)),(d__4__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 22, file GrammarParser.grm, line 89 *)
val _ = update_ yyact 22
(fn () => repr(let
val d__1__ = peekVal 3 : string
val d__4__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("grammatical-rule",[mkTerm((d__1__)),(d__4__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 23, file GrammarParser.grm, line 93 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
( mkNonTerm("identifier-list",[mkTerm((d__1__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 24, file GrammarParser.grm, line 94 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
val d__2__ = peekVal 0 : string
in
( mkNonTerm("identifier-list",[(d__1__),mkTerm((d__2__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 25, file GrammarParser.grm, line 98 *)
val _ = update_ yyact 25
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("alternate-list",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 26, file GrammarParser.grm, line 99 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__1__ = peekVal 2 : GrammarSyntax.Tree
val d__3__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("alternate-list",[(d__1__),(d__3__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 27, file GrammarParser.grm, line 103 *)
val _ = update_ yyact 27
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("alternate",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 28, file GrammarParser.grm, line 104 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("alternate",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 29, file GrammarParser.grm, line 108 *)
val _ = update_ yyact 29
(fn () => repr(let
in
( mkNonTerm("empty-alternate",[mkTerm("ε")]) ) end : GrammarSyntax.Tree))
;
(* Rule 30, file GrammarParser.grm, line 112 *)
val _ = update_ yyact 30
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("symbol-list",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 31, file GrammarParser.grm, line 113 *)
val _ = update_ yyact 31
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
val d__2__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("symbol-list",[(d__1__),(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 32, file GrammarParser.grm, line 117 *)
val _ = update_ yyact 32
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("symbol",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 33, file GrammarParser.grm, line 118 *)
val _ = update_ yyact 33
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
val d__2__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("symbol",[(d__1__),(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 34, file GrammarParser.grm, line 119 *)
val _ = update_ yyact 34
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("symbol",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 35, file GrammarParser.grm, line 120 *)
val _ = update_ yyact 35
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
val d__2__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("symbol",[(d__1__),(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 36, file GrammarParser.grm, line 124 *)
val _ = update_ yyact 36
(fn () => repr(let
in
( mkNonTerm("optional-marker",[mkTerm("[opt]")]) ) end : GrammarSyntax.Tree))
;
(* Rule 37, file GrammarParser.grm, line 128 *)
val _ = update_ yyact 37
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
( mkNonTerm("literal-symbol",[mkTerm((d__1__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 38, file GrammarParser.grm, line 132 *)
val _ = update_ yyact 38
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
( mkNonTerm("identifier",[mkTerm((d__1__))]) ) end : GrammarSyntax.Tree))
;
(* Entry File *)
val _ = update_ yyact 39 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun File lexer lexbuf = yyparse yytables 1 lexer lexbuf;
