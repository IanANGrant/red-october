{
open Lexing GrammarParser;

exception LexicalError of string * int * int

fun lexerError lexbuf s = 
    raise LexicalError (s, getLexemeStart lexbuf, getLexemeEnd lexbuf);

exception NotFound of string;

fun identifier lexbuf =
  let val s = getLexeme lexbuf
      val size' = String.size s - 1
  in if (String.sub (s,size')) = #":" 
        then RULE_IDENTIFIER (String.substring (s,0,size'))
        else if s = "one" then ONE 
        else if s = "of" then OF 
        else IDENTIFIER s
     handle Subscript => raise Fail ("Subscript exception in identifier: "^s)
  end;

fun literal lexbuf =
  let val s = getLexeme lexbuf
  in LITERAL (String.substring (s,1,String.size s - 2))
     handle Subscript => raise Fail ("Subscript exception in literal: "^s)
  end;

fun terminal lexbuf =
  let val s = getLexeme lexbuf
  in TERMINAL (String.substring (s,2,String.size s - 3))
     handle Subscript => raise Fail ("Subscript exception in terminal: "^s)
  end;

}

rule Token = parse
    [` ` `\t` `\n`]+    { Token lexbuf }
  | "'" [^`'`]+ "'"    { literal lexbuf } 
  | "\"" [^`\034`]+ "\""  { literal lexbuf } 
  | "<" "'" [^`'`]+ "'"   { terminal lexbuf } 
  | "<" "\"" [^`\034`]+ "\"" { terminal lexbuf } 
  | "[opt]"             { OPT }
  | `:`                 { COLON }
  | "\206\181"          { EPSILON }
  | `;`                 { SEMICOLON }
  | `|`                 { BAR }
  | [`A`-`Z``a`-`z`]
    [`A`-`Z``a`-`z``-``0`-`9`]*
    `:`?                        { identifier lexbuf } 
  | eof                         { EOF }
  | _ { lexerError lexbuf "Illegal symbol in input" }
;
