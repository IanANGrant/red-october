{
open Lexing RewriteParser;

exception LexicalError of string * int * int

fun lexerError lexbuf s = 
    raise LexicalError (s, getLexemeStart lexbuf, getLexemeEnd lexbuf);

exception NotFound of string;

fun identifier lexbuf =
  let val s = getLexeme lexbuf
  in IDENTIFIER s
  end;

fun literal lexbuf =
  let val s = getLexeme lexbuf
  in LITERAL s
  end;

}

rule Token = parse
    [` ` `\t` `\n`]+      { Token lexbuf }
  | "'" [^`'`]+ "'"       { literal lexbuf } 
  | "\"" [^`\034`]+ "\""  { literal lexbuf } 
  | "⌜"                   { LCORNER }
  | "⌝"                   { RCORNER }
  | "⟦"                   { DLBRACK }
  | "⟧"                   { DRBRACK }
  | "⟪"                   { DLANGLE }
  | "⟫"                   { DRANGLE }
  | "("                   { LPAR }
  | ")"                   { RPAR }
  | `:`                   { COLON }
  | `=`                   { EQUALS }
  | `_`                   { UNDERSCORE }
  | `*`                   { STAR }
  | "ε"                   { EPSILON }
  | [`A`-`Z``a`-`z`]
    [`A`-`Z``a`-`z``-``0`-`9`]*
                          { identifier lexbuf } 
  | eof                   { EOF }
  | _ { lexerError lexbuf "Illegal symbol in input" }
;
