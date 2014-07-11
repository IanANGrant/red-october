{
open Lexing CPPInfoParser;

exception LexicalError of string * int * int

fun lexerError lexbuf s = 
    raise LexicalError (s, getLexemeStart lexbuf, getLexemeEnd lexbuf);


fun integer lexbuf =
  let val s = getLexeme lexbuf
  in case Int.fromString s of
          NONE   => lexerError lexbuf "internal error: bad integer handling!"
        | SOME i => INTEGER i
  end;

fun string lexbuf =
  let val s = getLexeme lexbuf
  in STRING (String.substring (s, 1, (String.size s) - 2))
  end handle Subscript => lexerError lexbuf "internal error: bad string handling!";

}

rule Token = parse
    [` ` `\t` `\n` `\r`]
        { Token lexbuf }
  | `-`?[`0`-`9`]+
        { integer lexbuf }
  | `"` [`a`-`z``A`-`Z``0`-`9``_``.``/``<``>``-`]* `"`
        { string lexbuf }
  | `#` { HASH }
  | eof { EOF }
  | _   { lexerError lexbuf "Illegal symbol in input" }
;

