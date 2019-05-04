type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | LP
  | RP
  | COLON
  | EQ
  | RETURN
  | EOF
  | COMMA
  | STACK
  | PROCEDURES
  | STATIC
  | VARIABLES
  | EOL
  | OPTIONS

val act_parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A6.actions
