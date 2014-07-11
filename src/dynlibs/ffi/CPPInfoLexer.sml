local open Obj Lexing in


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


fun action_5 lexbuf = (
 lexerError lexbuf "Illegal symbol in input" )
and action_4 lexbuf = (
 EOF )
and action_3 lexbuf = (
 HASH )
and action_2 lexbuf = (
 string lexbuf )
and action_1 lexbuf = (
 integer lexbuf )
and action_0 lexbuf = (
 Token lexbuf )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_7 lexbuf
 else case currChar of
    #"\n" => action_0 lexbuf
 |  #"\t" => action_0 lexbuf
 |  #"\r" => action_0 lexbuf
 |  #" " => action_0 lexbuf
 |  #"-" => state_6 lexbuf
 |  #"#" => action_3 lexbuf
 |  #"\"" => state_4 lexbuf
 |  #"\^@" => action_4 lexbuf
 |  _ => action_5 lexbuf
 end)
and state_4 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"-" andalso currChar <= #"9" then  state_10 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_10 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_10 lexbuf
 else case currChar of
    #"<" => state_10 lexbuf
 |  #">" => state_10 lexbuf
 |  #"_" => state_10 lexbuf
 |  #"\"" => action_2 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_6 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_8 lexbuf
 else backtrack lexbuf
 end)
and state_7 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_1);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_8 lexbuf
 else backtrack lexbuf
 end)
and state_8 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_1);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_8 lexbuf
 else backtrack lexbuf
 end)
and state_10 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"-" andalso currChar <= #"9" then  state_10 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_10 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_10 lexbuf
 else case currChar of
    #"<" => state_10 lexbuf
 |  #">" => state_10 lexbuf
 |  #"_" => state_10 lexbuf
 |  #"\"" => action_2 lexbuf
 |  _ => backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_5, action_4, action_3, action_2, action_1, action_0];

end
