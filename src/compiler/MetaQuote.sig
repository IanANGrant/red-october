val is_quotedef_name : string -> bool;
val quotedef_decl : string -> (Lexing.lexbuf -> Parser.token) * string;
val new_quotedef_name : string -> (Lexing.lexbuf -> Parser.token) * string -> unit;

