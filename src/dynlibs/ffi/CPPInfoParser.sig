local
in
datatype token =
    EOF
  | HASH
  | INTEGER of int
  | STRING of string
end;

val CPPInfo :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> CSyntax.Tree;
