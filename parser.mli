type token =
  | LPAREN
  | RPAREN
  | LSQUARE
  | RSQUARE
  | COMMA
  | ARROW
  | COLON
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | EXP
  | ADDASN
  | SUBASN
  | MULASN
  | DIVASN
  | MODASN
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | RANGE
  | AND
  | OR
  | IF
  | ELSEIF
  | ELSE
  | FOR
  | IN
  | DO
  | WHILE
  | BREAK
  | CONTINUE
  | RETURN
  | END
  | INT
  | FLOAT
  | BOOL
  | STRING
  | CHAR
  | VOID
  | LIST
  | BLIT of (bool)
  | LITERAL of (int)
  | FLIT of (float)
  | ID of (string)
  | CLIT of (char)
  | SLIT of (string)
  | EOL
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
