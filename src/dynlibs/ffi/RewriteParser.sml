local
in
datatype token =
    COLON
  | DLANGLE
  | DLBRACK
  | DRANGLE
  | DRBRACK
  | EOF
  | EPSILON
  | EQUALS
  | IDENTIFIER of string
  | LCORNER
  | LITERAL of string
  | LPAR
  | RCORNER
  | RPAR
  | STAR
  | UNDERSCORE
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

open GrammarSyntax
(* Line 7, file RewriteParser.sml *)
val yytransl = #[
  257 (* COLON *),
  258 (* DLANGLE *),
  259 (* DLBRACK *),
  260 (* DRANGLE *),
  261 (* DRBRACK *),
  262 (* EOF *),
  263 (* EPSILON *),
  264 (* EQUALS *),
  265 (* IDENTIFIER *),
  266 (* LCORNER *),
  267 (* LITERAL *),
  268 (* LPAR *),
  269 (* RCORNER *),
  270 (* RPAR *),
  271 (* STAR *),
  272 (* UNDERSCORE *),
    0];

val yylhs = "\255\255\
\\001\000\001\000\014\000\014\000\013\000\012\000\012\000\012\000\
\\011\000\011\000\011\000\005\000\005\000\006\000\006\000\007\000\
\\007\000\007\000\008\000\008\000\008\000\008\000\008\000\009\000\
\\009\000\009\000\010\000\010\000\010\000\010\000\004\000\004\000\
\\002\000\003\000\000\000";

val yylen = "\002\000\
\\002\000\001\000\001\000\002\000\007\000\002\000\001\000\001\000\
\\002\000\001\000\001\000\001\000\002\000\001\000\002\000\001\000\
\\003\000\001\000\003\000\008\000\005\000\003\000\005\000\001\000\
\\003\000\001\000\003\000\006\000\003\000\003\000\001\000\001\000\
\\001\000\001\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\002\000\035\000\003\000\000\000\000\000\
\\000\000\008\000\000\000\001\000\004\000\033\000\034\000\000\000\
\\031\000\032\000\018\000\000\000\012\000\016\000\000\000\000\000\
\\000\000\000\000\013\000\000\000\019\000\000\000\022\000\000\000\
\\017\000\000\000\000\000\000\000\000\000\000\000\021\000\023\000\
\\000\000\000\000\000\000\011\000\000\000\000\000\000\000\000\000\
\\026\000\000\000\014\000\024\000\000\000\000\000\005\000\000\000\
\\027\000\000\000\015\000\029\000\000\000\030\000\020\000\025\000\
\\000\000\000\000\028\000";

val yydgoto = "\002\000\
\\005\000\017\000\018\000\019\000\020\000\050\000\021\000\010\000\
\\051\000\044\000\045\000\011\000\006\000\007\000";

val yysindex = "\011\000\
\\003\255\000\000\039\255\000\000\000\000\000\000\004\255\031\255\
\\030\255\000\000\022\255\000\000\000\000\000\000\000\000\039\255\
\\000\000\000\000\000\000\031\255\000\000\000\000\001\255\002\255\
\\036\255\040\255\000\000\042\255\000\000\043\255\000\000\041\255\
\\000\000\044\255\045\255\046\255\028\255\039\255\000\000\000\000\
\\028\255\008\255\038\255\000\000\047\255\048\255\050\255\028\255\
\\000\000\008\255\000\000\000\000\051\255\017\255\000\000\052\255\
\\000\000\049\255\000\000\000\000\028\255\000\000\000\000\000\000\
\\053\255\055\255\000\000";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\255\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\019\255\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\020\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\021\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000";

val yygindex = "\000\000\
\\000\000\000\000\000\000\219\255\000\000\000\000\037\000\252\255\
\\010\000\242\255\216\255\240\255\059\000\000\000";

val YYTABLESIZE = 68;
val yytable = "\026\000\
\\047\000\028\000\030\000\022\000\049\000\003\000\003\000\058\000\
\\004\000\012\000\041\000\001\000\049\000\029\000\031\000\022\000\
\\014\000\043\000\015\000\048\000\065\000\046\000\007\000\006\000\
\\010\000\009\000\025\000\052\000\061\000\062\000\041\000\007\000\
\\006\000\010\000\009\000\052\000\042\000\043\000\023\000\014\000\
\\009\000\015\000\016\000\032\000\053\000\024\000\054\000\008\000\
\\009\000\034\000\035\000\036\000\037\000\033\000\057\000\038\000\
\\027\000\039\000\040\000\059\000\055\000\056\000\064\000\060\000\
\\063\000\013\000\066\000\067\000";

val yycheck = "\016\000\
\\041\000\001\001\001\001\008\000\042\000\003\001\003\001\048\000\
\\006\001\006\001\003\001\001\000\050\000\013\001\013\001\020\000\
\\009\001\010\001\011\001\012\001\061\000\038\000\005\001\005\001\
\\005\001\005\001\005\001\042\000\012\001\013\001\003\001\014\001\
\\014\001\014\001\014\001\050\000\009\001\010\001\009\001\009\001\
\\010\001\011\001\012\001\008\001\007\001\016\001\009\001\009\001\
\\010\001\008\001\009\001\009\001\012\001\014\001\005\001\012\001\
\\020\000\013\001\013\001\050\000\014\001\014\001\014\001\013\001\
\\013\001\007\000\014\001\013\001";

val yyact = vector_ 36 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file RewriteParser.grm, line 38 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
in
( (d__1__) ) end : GrammarSyntax.Tree))
;
(* Rule 2, file RewriteParser.grm, line 39 *)
val _ = update_ yyact 2
(fn () => repr(let
in
( mkNonTerm("rule-list",[]) ) end : GrammarSyntax.Tree))
;
(* Rule 3, file RewriteParser.grm, line 43 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("rule-list", [(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 4, file RewriteParser.grm, line 44 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
val d__2__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("rule-list", [(d__1__),(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 5, file RewriteParser.grm, line 48 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__2__ = peekVal 5 : GrammarSyntax.Tree
val d__6__ = peekVal 1 : GrammarSyntax.Tree
in
( mkNonTerm("rule",[(d__2__),(d__6__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 6, file RewriteParser.grm, line 52 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 1 : string
val d__2__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("rule-lhs",[mkTerm((d__1__)),(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 7, file RewriteParser.grm, line 53 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
( mkNonTerm("rule-lhs",[mkTerm((d__1__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 8, file RewriteParser.grm, line 54 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("rule-lhs",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 9, file RewriteParser.grm, line 58 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 1 : string
val d__2__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("rule-rhs",[mkTerm((d__1__)),(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 10, file RewriteParser.grm, line 59 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
( mkNonTerm("rule-rhs",[mkTerm((d__1__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 11, file RewriteParser.grm, line 60 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("rule-rhs",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 12, file RewriteParser.grm, line 64 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("lhs-arg-list",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 13, file RewriteParser.grm, line 65 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
val d__2__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("lhs-arg-list",[(d__1__),(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 14, file RewriteParser.grm, line 69 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("rhs-arg-list",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 15, file RewriteParser.grm, line 70 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 1 : GrammarSyntax.Tree
val d__2__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("rhs-arg-list",[(d__1__),(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 16, file RewriteParser.grm, line 74 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("lhs-arg",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 17, file RewriteParser.grm, line 75 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__2__ = peekVal 1 : GrammarSyntax.Tree
in
( mkNonTerm("lhs-arg",[(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 18, file RewriteParser.grm, line 76 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("lhs-arg",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 19, file RewriteParser.grm, line 80 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__2__ = peekVal 1 : string
in
( mkNonTerm("lhs-metaquote",[mkTerm((d__2__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 20, file RewriteParser.grm, line 81 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__2__ = peekVal 6 : string
val d__6__ = peekVal 2 : GrammarSyntax.Tree
in
( mkNonTerm("lhs-metaquote",[mkTerm((d__2__)),(d__6__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 21, file RewriteParser.grm, line 82 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__2__ = peekVal 3 : string
val d__4__ = peekVal 1 : string
in
( mkNonTerm("lhs-metaquote",[mkTerm((d__2__)),mkTerm((d__4__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 22, file RewriteParser.grm, line 83 *)
val _ = update_ yyact 22
(fn () => repr(let
in
( mkNonTerm("lhs-metaquote",[mkTerm("_")]) ) end : GrammarSyntax.Tree))
;
(* Rule 23, file RewriteParser.grm, line 84 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__4__ = peekVal 1 : string
in
( mkNonTerm("lhs-metaquote",[mkTerm("_"),mkTerm((d__4__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 24, file RewriteParser.grm, line 88 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("rhs-arg",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 25, file RewriteParser.grm, line 89 *)
val _ = update_ yyact 25
(fn () => repr(let
val d__2__ = peekVal 1 : GrammarSyntax.Tree
in
( mkNonTerm("rhs-arg",[(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 26, file RewriteParser.grm, line 90 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("rhs-arg",[(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 27, file RewriteParser.grm, line 94 *)
val _ = update_ yyact 27
(fn () => repr(let
val d__2__ = peekVal 1 : GrammarSyntax.Tree
in
( mkNonTerm("rhs-meta-quote",[(d__2__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 28, file RewriteParser.grm, line 95 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__2__ = peekVal 4 : string
val d__4__ = peekVal 2 : GrammarSyntax.Tree
in
( mkNonTerm("rhs-meta-quote",[mkTerm((d__2__)),(d__4__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 29, file RewriteParser.grm, line 96 *)
val _ = update_ yyact 29
(fn () => repr(let
in
( mkNonTerm("rhs-meta-quote",[mkNonTerm("",[])]) ) end : GrammarSyntax.Tree))
;
(* Rule 30, file RewriteParser.grm, line 97 *)
val _ = update_ yyact 30
(fn () => repr(let
val d__2__ = peekVal 1 : string
in
( mkNonTerm("rhs-meta-quote",[mkTerm((d__2__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 31, file RewriteParser.grm, line 101 *)
val _ = update_ yyact 31
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("at-arg", [(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 32, file RewriteParser.grm, line 102 *)
val _ = update_ yyact 32
(fn () => repr(let
val d__1__ = peekVal 0 : GrammarSyntax.Tree
in
( mkNonTerm("at-arg", [(d__1__)]) ) end : GrammarSyntax.Tree))
;
(* Rule 33, file RewriteParser.grm, line 106 *)
val _ = update_ yyact 33
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
( mkNonTerm("identifier", [mkTerm((d__1__))]) ) end : GrammarSyntax.Tree))
;
(* Rule 34, file RewriteParser.grm, line 110 *)
val _ = update_ yyact 34
(fn () => repr(let
val d__1__ = peekVal 0 : string
in
( mkNonTerm("literal", [mkTerm((d__1__))]) ) end : GrammarSyntax.Tree))
;
(* Entry File *)
val _ = update_ yyact 35 (fn () => raise yyexit (peekVal 0));
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
