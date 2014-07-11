local
in
datatype token =
    BAR
  | COLON
  | EOF
  | EPSILON
  | EQUALS
  | IDENTIFIER of string
  | LITERAL of string
  | OF
  | ONE
  | OPT
  | RULE_IDENTIFIER of string
  | SEMICOLON
  | TERMINAL of string
end;

val File :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> GrammarSyntax.Tree;
