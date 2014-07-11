local
in
datatype token =
    EOF
  | HASH
  | INTEGER of int
  | STRING of string
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

open CSyntax
(* Line 7, file CPPInfoParser.sml *)
val yytransl = #[
  257 (* EOF *),
  258 (* HASH *),
  259 (* INTEGER *),
  260 (* STRING *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\000\000";

val yylen = "\002\000\
\\005\000\002\000\000\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\
\\002\000\001\000";

val yydgoto = "\002\000\
\\004\000\008\000";

val yysindex = "\255\255\
\\255\254\000\000\000\255\000\000\254\254\001\255\001\255\004\255\
\\000\000\000\000";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\005\255\005\255\000\000\
\\000\000\000\000";

val yygindex = "\000\000\
\\000\000\001\000";

val YYTABLESIZE = 8;
val yytable = "\001\000\
\\003\000\006\000\005\000\007\000\010\000\003\000\000\000\009\000";

val yycheck = "\001\000\
\\002\001\004\001\003\001\003\001\001\001\001\001\255\255\007\000";

val yyact = vector_ 5 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file CPPInfoParser.grm, line 17 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__2__ = peekVal 3 : int
val d__3__ = peekVal 2 : string
val d__4__ = peekVal 1 : CSyntax.Tree
in
( mkNonTerm("cpp-info",[mkTerm(Int.toString((d__2__))),mkTerm((d__3__)),(d__4__)]) ) end : CSyntax.Tree))
;
(* Rule 2, file CPPInfoParser.grm, line 21 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 1 : int
val d__2__ = peekVal 0 : CSyntax.Tree
in
( mkNonTerm("int-list",[mkTerm(Int.toString((d__1__))),(d__2__)]) ) end : CSyntax.Tree))
;
(* Rule 3, file CPPInfoParser.grm, line 22 *)
val _ = update_ yyact 3
(fn () => repr(let
in
( mkNonTerm("int-list",[]) ) end : CSyntax.Tree))
;
(* Entry CPPInfo *)
val _ = update_ yyact 4 (fn () => raise yyexit (peekVal 0));
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
fun CPPInfo lexer lexbuf = yyparse yytables 1 lexer lexbuf;
