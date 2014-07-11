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

val File :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> GrammarSyntax.Tree;
