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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open A6                       
# 24 "parser.ml"
let yytransl_const = [|
  260 (* LP *);
  261 (* RP *);
  262 (* COLON *);
  263 (* EQ *);
  264 (* RETURN *);
    0 (* EOF *);
  265 (* COMMA *);
  266 (* STACK *);
  267 (* PROCEDURES *);
  268 (* STATIC *);
  269 (* VARIABLES *);
  270 (* EOL *);
  271 (* OPTIONS *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\004\000\004\000\005\000\005\000\006\000\006\000\008\000\007\000\
\007\000\007\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\004\000\001\000\003\000\001\000\001\000\004\000\001\000\001\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\015\000\003\000\004\000\005\000\006\000\
\002\000\019\000\000\000\007\000\008\000\010\000\013\000\000\000\
\000\000\001\000\016\000\018\000\017\000\000\000\000\000\000\000\
\009\000\000\000\014\000\011\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000\022\000\014\000\023\000\015\000"

let yysindex = "\003\000\
\253\254\000\000\007\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\248\254\000\000\000\000\000\000\000\000\000\255\
\008\255\000\000\000\000\000\000\000\000\009\255\010\255\000\255\
\000\000\000\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\011\255\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\247\255\000\000\250\255\000\000"

let yytablesize = 19
let yytable = "\003\000\
\019\000\020\000\021\000\001\000\004\000\018\000\005\000\006\000\
\007\000\008\000\016\000\009\000\017\000\025\000\024\000\012\000\
\028\000\027\000\026\000"

let yycheck = "\003\001\
\001\001\002\001\003\001\001\000\008\001\014\001\010\001\011\001\
\012\001\013\001\004\001\015\001\006\001\005\001\007\001\005\001\
\026\000\024\000\009\001"

let yynames_const = "\
  LP\000\
  RP\000\
  COLON\000\
  EQ\000\
  RETURN\000\
  EOF\000\
  COMMA\000\
  STACK\000\
  PROCEDURES\000\
  STATIC\000\
  VARIABLES\000\
  EOL\000\
  OPTIONS\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'basic_actions) in
    Obj.repr(
# 12 "parser.mly"
                      (_1)
# 121 "parser.ml"
               : A6.actions))
; (fun __caml_parser_env ->
    Obj.repr(
# 16 "parser.mly"
            (PrintOptions)
# 127 "parser.ml"
               : 'basic_actions))
; (fun __caml_parser_env ->
    Obj.repr(
# 17 "parser.mly"
           (PrintStk)
# 133 "parser.ml"
               : 'basic_actions))
; (fun __caml_parser_env ->
    Obj.repr(
# 18 "parser.mly"
                (PrintProc)
# 139 "parser.ml"
               : 'basic_actions))
; (fun __caml_parser_env ->
    Obj.repr(
# 19 "parser.mly"
            (PrintSL)
# 145 "parser.ml"
               : 'basic_actions))
; (fun __caml_parser_env ->
    Obj.repr(
# 20 "parser.mly"
               (PrintVar)
# 151 "parser.ml"
               : 'basic_actions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 21 "parser.mly"
                 (_1)
# 158 "parser.ml"
               : 'basic_actions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_call) in
    Obj.repr(
# 24 "parser.mly"
                  (_1)
# 165 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 27 "parser.mly"
                       (FunctionCall(_1,_3))
# 173 "parser.ml"
               : 'function_call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assign) in
    Obj.repr(
# 28 "parser.mly"
             (_1)
# 180 "parser.ml"
               : 'function_call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'basic) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 31 "parser.mly"
                            (_1::_3)
# 188 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'basic) in
    Obj.repr(
# 32 "parser.mly"
            ([_1])
# 195 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'return) in
    Obj.repr(
# 35 "parser.mly"
           (_1)
# 202 "parser.ml"
               : 'assign))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'basic) in
    Obj.repr(
# 36 "parser.mly"
                        (Command(_1,_4))
# 210 "parser.ml"
               : 'assign))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
           (Return)
# 216 "parser.ml"
               : 'return))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                                      (Integer(_1))
# 223 "parser.ml"
               : 'basic))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "parser.mly"
                                      (V(_1))
# 230 "parser.ml"
               : 'basic))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 44 "parser.mly"
                                  ((Bool(_1)))
# 237 "parser.ml"
               : 'basic))
(* Entry act_parser *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let act_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : A6.actions)
