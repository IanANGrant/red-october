local open Obj Lexing in


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


fun action_12 lexbuf = (
 lexerError lexbuf "Illegal symbol in input" )
and action_11 lexbuf = (
 EOF )
and action_10 lexbuf = (
 identifier lexbuf )
and action_9 lexbuf = (
 BAR )
and action_8 lexbuf = (
 SEMICOLON )
and action_7 lexbuf = (
 EPSILON )
and action_6 lexbuf = (
 COLON )
and action_5 lexbuf = (
 OPT )
and action_4 lexbuf = (
 terminal lexbuf )
and action_3 lexbuf = (
 terminal lexbuf )
and action_2 lexbuf = (
 literal lexbuf )
and action_1 lexbuf = (
 literal lexbuf )
and action_0 lexbuf = (
 Token lexbuf )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_9 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_9 lexbuf
 else case currChar of
    #"\n" => state_3 lexbuf
 |  #"\t" => state_3 lexbuf
 |  #" " => state_3 lexbuf
 |  #"\206" => state_12 lexbuf
 |  #"|" => action_9 lexbuf
 |  #"[" => state_10 lexbuf
 |  #"<" => state_8 lexbuf
 |  #";" => action_8 lexbuf
 |  #":" => action_6 lexbuf
 |  #"'" => state_5 lexbuf
 |  #"\"" => state_4 lexbuf
 |  #"\^@" => action_11 lexbuf
 |  _ => action_12 lexbuf
 end)
and state_3 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => state_30 lexbuf
 |  #"\t" => state_30 lexbuf
 |  #" " => state_30 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_4 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_12);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\"" => backtrack lexbuf
 |  _ => state_28 lexbuf
 end)
and state_5 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_12);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"'" => backtrack lexbuf
 |  _ => state_26 lexbuf
 end)
and state_8 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_12);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"'" => state_21 lexbuf
 |  #"\"" => state_20 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_9 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_18 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_18 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_18 lexbuf
 else case currChar of
    #"-" => state_18 lexbuf
 |  #":" => action_10 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_10 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_12);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"o" => state_14 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_12 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_12);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\181" => action_7 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_14 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"p" => state_15 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_15 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"t" => state_16 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_16 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"]" => action_5 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_18 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_18 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_18 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_18 lexbuf
 else case currChar of
    #"-" => state_18 lexbuf
 |  #":" => action_10 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_20 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\"" => backtrack lexbuf
 |  _ => state_24 lexbuf
 end)
and state_21 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"'" => backtrack lexbuf
 |  _ => state_22 lexbuf
 end)
and state_22 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"'" => action_3 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_22 lexbuf
 end)
and state_24 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\"" => action_4 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_24 lexbuf
 end)
and state_26 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"'" => action_1 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_26 lexbuf
 end)
and state_28 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\"" => action_2 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_28 lexbuf
 end)
and state_30 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => state_30 lexbuf
 |  #"\t" => state_30 lexbuf
 |  #" " => state_30 lexbuf
 |  _ => backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_12, action_11, action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
