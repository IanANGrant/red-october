local
  open Mixture Globals Asynt;
in

val createLexerStream : BasicIO.instream -> Lexing.lexbuf;
        (* Create a lexer buffer on the given input channel.
           [createLexerStream inchan] returns a lexer buffer which reads
           from the input channel [inchan], at the current reading position. *)

val createLexerString : string -> Lexing.lexbuf;

val parseToplevelPhrase : Lexing.lexbuf -> Dec * bool;
val parseTopDecPhrase : Lexing.lexbuf -> Struct;
val elabToplevelDecPhrase : Dec -> (string, InfixStatus) Env * 
                                  (ModEnv * FunEnv * SigEnv * VarEnv * TyEnv) Existential;
val cleanEnv : (''_a, 'b) Env -> (''_a * 'b) list;
val reportFixityResult : string * InfixStatus -> unit;
val verbose : bool ref;
val compileSignature : (string list) -> string -> Mode -> string -> unit;
val compileUnitBody : (string list) -> string -> Mode -> string -> unit;

end;
