{
open Lexing PrologParser;

exception LexicalError of string * int * int

fun lexerError lexbuf s = 
    raise LexicalError (s, getLexemeStart lexbuf, getLexemeEnd lexbuf);

exception NotFound of string;

fun variable lexbuf =
  let val s = getLexeme lexbuf
  in VARIABLE s
  end;

fun constant lexbuf =
  let val s = getLexeme lexbuf
  in CONSTANT s
  end;

}

rule Token = parse
    [` ` `\t` `\n`]+                            { Token lexbuf }
  | ":-"                                        { HEAD } 
  | `(`                                         { LPAR } 
  | `)`                                         { RPAR } 
  | `[`                                         { LBRACK } 
  | `]`                                         { RBRACK } 
  | `|`                                         { BAR } 
  | `,`                                         { COMMA } 
  | `.`                                         { DOT } 
  | [`a`-`z``0`-`9`][`A`-`Z``a`-`z``_``0`-`9`]* { constant lexbuf } 
  | [`A`-`Z``_`][`A`-`Z``a`-`z``_``0`-`9`]*     { variable lexbuf } 
  | eof                                         { EOF }
  | [^`A`-`Z``a`-`z``_``0`-`9`` ` `\t` `\n``(``)``,``.``[``]``|`]+   { constant lexbuf } 
  | _ { lexerError lexbuf "Unexpected symbol in input" }
;
