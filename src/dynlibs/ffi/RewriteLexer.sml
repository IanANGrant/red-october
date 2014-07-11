local open Obj Lexing in


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


fun action_18 lexbuf = (
 lexerError lexbuf "Illegal symbol in input" )
and action_17 lexbuf = (
 EOF )
and action_16 lexbuf = (
 identifier lexbuf )
and action_15 lexbuf = (
 EPSILON )
and action_14 lexbuf = (
 STAR )
and action_13 lexbuf = (
 UNDERSCORE )
and action_12 lexbuf = (
 EQUALS )
and action_11 lexbuf = (
 COLON )
and action_10 lexbuf = (
 RPAR )
and action_9 lexbuf = (
 LPAR )
and action_8 lexbuf = (
 DRANGLE )
and action_7 lexbuf = (
 DLANGLE )
and action_6 lexbuf = (
 DRBRACK )
and action_5 lexbuf = (
 DLBRACK )
and action_4 lexbuf = (
 RCORNER )
and action_3 lexbuf = (
 LCORNER )
and action_2 lexbuf = (
 literal lexbuf )
and action_1 lexbuf = (
 literal lexbuf )
and action_0 lexbuf = (
 Token lexbuf )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_11 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_11 lexbuf
 else case currChar of
    #"\n" => state_3 lexbuf
 |  #"\t" => state_3 lexbuf
 |  #" " => state_3 lexbuf
 |  #"\226" => state_14 lexbuf
 |  #"\206" => state_13 lexbuf
 |  #"_" => action_13 lexbuf
 |  #"=" => action_12 lexbuf
 |  #":" => action_11 lexbuf
 |  #"*" => action_14 lexbuf
 |  #")" => action_10 lexbuf
 |  #"(" => action_9 lexbuf
 |  #"'" => state_5 lexbuf
 |  #"\"" => state_4 lexbuf
 |  #"\^@" => action_17 lexbuf
 |  _ => action_18 lexbuf
 end)
and state_3 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => state_29 lexbuf
 |  #"\t" => state_29 lexbuf
 |  #" " => state_29 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_4 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_18);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\"" => backtrack lexbuf
 |  _ => state_27 lexbuf
 end)
and state_5 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_18);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"'" => backtrack lexbuf
 |  _ => state_25 lexbuf
 end)
and state_11 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_16);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_24 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_24 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"-" => state_24 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_13 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_18);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\181" => action_15 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_14 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_18);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\159" => state_16 lexbuf
 |  #"\140" => state_15 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_15 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\157" => action_4 lexbuf
 |  #"\156" => action_3 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_16 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\171" => action_8 lexbuf
 |  #"\170" => action_7 lexbuf
 |  #"\167" => action_6 lexbuf
 |  #"\166" => action_5 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_24 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_16);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_24 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_24 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_24 lexbuf
 else case currChar of
    #"-" => state_24 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_25 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"'" => action_1 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_25 lexbuf
 end)
and state_27 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\"" => action_2 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_27 lexbuf
 end)
and state_29 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => state_29 lexbuf
 |  #"\t" => state_29 lexbuf
 |  #" " => state_29 lexbuf
 |  _ => backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_18, action_17, action_16, action_15, action_14, action_13, action_12, action_11, action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
