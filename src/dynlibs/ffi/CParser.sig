local
in
datatype token =
    ADDASSIGN
  | ALIGNAS
  | AND
  | ANDASSIGN
  | ATOMIC
  | AUTO
  | BOOL
  | BREAK
  | CASE
  | CHAR
  | CHARACTER of string
  | COLON
  | COMMA
  | COMPLEX
  | CONST
  | CONTINUE
  | CPPELSE
  | CPPIF
  | DEC
  | DECIMAL_POINT
  | DECINT of string
  | DEFAULT
  | DEFINE
  | DEFINED
  | DIGIT_SEQUENCE of string
  | DIVIDE
  | DIVIDEASSIGN
  | DO
  | DOUBLE
  | E
  | ELIF
  | ELIPSIS
  | ELSE
  | ENDIF
  | ENUM
  | EOF
  | EOL
  | EQ
  | EQUALS
  | ERROR
  | EXTERN
  | FLOAT
  | FLOATING_SUFFIX of string
  | FOR
  | GE
  | GENERIC
  | GOTO
  | GT
  | HEXINT of string
  | IDENTIFIER of string
  | IF
  | IFDEF
  | IFNDEF
  | INC
  | INCLUDE of string
  | INLINE
  | INT
  | LAND
  | LBRACE
  | LBRACK
  | LE
  | LNOT
  | LONG
  | LONG_SUFFIX
  | LOR
  | LPAR
  | LS
  | LSASSIGN
  | LT
  | MINUS
  | MINUSASSIGN
  | MOD
  | MODASSIGN
  | MULTIPLYASSIGN
  | NE
  | NI
  | NOT
  | NO_RETURN
  | OCTINT of string
  | OR
  | ORASSIGN
  | PLUS
  | PMIDENTIFIER of string
  | PRAGMA
  | RBRACE
  | RBRACK
  | REGISTER
  | RESTRICT
  | RETURN
  | RPAR
  | RS
  | RSASSIGN
  | SEMICOLON
  | SHORT
  | SIGN of string
  | SIGNED
  | SIZEOF
  | SPR
  | SR
  | STAR
  | STATIC
  | STATIC_ASSERT
  | STRING of string
  | STRUCT
  | SWITCH
  | SYSINCLUDE of string
  | TEXT_FRAG of string
  | THEN
  | THREAD_LOCAL
  | TOKEN_PASTE
  | TYPEDEF
  | TYPEDEF_NAME of string
  | UNDEF
  | UNION
  | UNSIGNED
  | UNSIGNED_SUFFIX
  | VOID
  | VOLATILE
  | WARNING
  | WHILE
  | XOR
  | XORASSIGN
end;

val CPPFile :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> CSyntax.Tree;
val File :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> CSyntax.Tree;
