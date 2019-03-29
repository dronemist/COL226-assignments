type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | ABS
  | TILDA
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | REM
  | CONJ
  | DISJ
  | EQ
  | GT
  | LT
  | LP
  | RP
  | IF
  | THEN
  | ELSE
  | FI
  | COMMA
  | PROJ
  | LET
  | IN
  | END
  | BACKSLASH
  | DOT
  | DEF
  | SEMICOLON
  | PARALLEL
  | LOCAL
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "a3.mly"
    open A1                            
# 42 "a3.ml"
let yytransl_const = [|
  260 (* ABS *);
  261 (* TILDA *);
  262 (* NOT *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIV *);
  267 (* REM *);
  268 (* CONJ *);
  269 (* DISJ *);
  270 (* EQ *);
  271 (* GT *);
  272 (* LT *);
  273 (* LP *);
  274 (* RP *);
  275 (* IF *);
  276 (* THEN *);
  277 (* ELSE *);
  278 (* FI *);
  279 (* COMMA *);
  280 (* PROJ *);
  281 (* LET *);
  282 (* IN *);
  283 (* END *);
  284 (* BACKSLASH *);
  285 (* DOT *);
  286 (* DEF *);
  287 (* SEMICOLON *);
  288 (* PARALLEL *);
  289 (* LOCAL *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\002\000\003\000\003\000\004\000\004\000\005\000\005\000\006\000\
\006\000\006\000\006\000\006\000\006\000\007\000\007\000\007\000\
\008\000\008\000\008\000\008\000\009\000\009\000\009\000\010\000\
\010\000\010\000\010\000\010\000\011\000\011\000\012\000\012\000\
\014\000\014\000\014\000\015\000\015\000\016\000\016\000\016\000\
\001\000\013\000\013\000\013\000\017\000\020\000\020\000\020\000\
\018\000\021\000\021\000\021\000\019\000\019\000\000\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\003\000\001\000\002\000\001\000\003\000\
\003\000\003\000\004\000\004\000\001\000\003\000\003\000\001\000\
\003\000\003\000\003\000\001\000\002\000\002\000\001\000\007\000\
\007\000\002\000\003\000\001\000\003\000\003\000\005\000\001\000\
\004\000\004\000\001\000\001\000\003\000\001\000\001\000\001\000\
\002\000\001\000\001\000\001\000\003\000\001\000\001\000\003\000\
\003\000\001\000\001\000\003\000\005\000\004\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\055\000\000\000\000\000\
\000\000\000\000\000\000\000\000\038\000\040\000\039\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\056\000\
\000\000\000\000\005\000\000\000\000\000\000\000\020\000\023\000\
\028\000\000\000\035\000\036\000\000\000\000\000\041\000\000\000\
\000\000\021\000\022\000\006\000\026\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\037\000\000\000\027\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\017\000\018\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\033\000\000\000\000\000\034\000\053\000\000\000\000\000\031\000\
\000\000\000\000\000\000\024\000\025\000"

let yydgoto = "\003\000\
\006\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\047\000\033\000\007\000\034\000\035\000\036\000\
\008\000\009\000\010\000\011\000\012\000"

let yysindex = "\030\000\
\244\254\065\255\000\000\005\255\244\254\000\000\019\000\000\000\
\000\000\000\000\025\255\030\255\000\000\000\000\000\000\156\255\
\156\255\065\255\009\255\065\255\046\255\244\254\062\255\000\000\
\005\000\063\255\000\000\099\255\112\255\113\255\000\000\000\000\
\000\000\064\255\000\000\000\000\074\255\073\255\000\000\244\254\
\244\254\000\000\000\000\000\000\000\000\017\255\036\255\029\255\
\090\255\075\255\076\255\065\255\000\000\065\255\156\255\093\255\
\128\255\156\255\156\255\156\255\156\255\156\255\065\255\065\255\
\244\254\000\000\000\000\000\000\065\255\000\000\065\255\065\255\
\086\255\065\255\022\255\063\255\000\000\112\255\156\255\112\255\
\156\255\112\255\113\255\113\255\000\000\000\000\000\000\028\255\
\098\255\089\255\098\255\098\255\023\255\125\255\245\254\065\255\
\000\000\112\255\112\255\000\000\000\000\065\255\110\255\000\000\
\061\255\016\255\169\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\146\000\
\020\000\062\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\221\000\000\000\206\000\085\000\022\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\083\000\104\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\236\000\000\000\106\000\000\000\127\000\
\000\000\148\000\043\000\064\000\000\000\000\000\000\000\000\000\
\125\000\000\000\069\255\085\255\000\000\000\000\000\000\000\000\
\000\000\169\000\190\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\240\255\084\000\247\255\000\000\021\000\014\000\
\246\255\031\000\000\000\000\000\251\255\000\000\065\000\000\000\
\000\000\000\000\094\000\000\000\000\000"

let yytablesize = 524
let yytable = "\038\000\
\032\000\052\000\046\000\048\000\053\000\042\000\043\000\037\000\
\044\000\013\000\014\000\015\000\016\000\017\000\018\000\104\000\
\050\000\004\000\039\000\043\000\005\000\016\000\013\000\014\000\
\015\000\019\000\045\000\020\000\052\000\052\000\001\000\002\000\
\021\000\022\000\068\000\052\000\023\000\108\000\096\000\069\000\
\052\000\052\000\015\000\102\000\077\000\100\000\088\000\089\000\
\072\000\085\000\086\000\087\000\091\000\070\000\092\000\093\000\
\040\000\095\000\071\000\090\000\041\000\044\000\049\000\014\000\
\051\000\013\000\014\000\015\000\016\000\017\000\018\000\083\000\
\084\000\052\000\054\000\078\000\080\000\082\000\068\000\105\000\
\063\000\019\000\045\000\020\000\013\000\106\000\030\000\064\000\
\021\000\022\000\073\000\030\000\023\000\013\000\014\000\015\000\
\016\000\017\000\065\000\098\000\074\000\099\000\029\000\049\000\
\075\000\008\000\079\000\029\000\094\000\019\000\052\000\020\000\
\055\000\056\000\057\000\101\000\021\000\022\000\058\000\059\000\
\023\000\060\000\061\000\062\000\054\000\103\000\009\000\107\000\
\013\000\014\000\015\000\016\000\017\000\066\000\067\000\076\000\
\000\000\109\000\000\000\097\000\000\000\081\000\000\000\000\000\
\019\000\042\000\020\000\010\000\000\000\000\000\000\000\021\000\
\022\000\000\000\000\000\023\000\013\000\014\000\015\000\016\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\000\013\000\014\000\015\000\019\000\000\000\020\000\000\000\
\000\000\000\000\000\000\021\000\022\000\000\000\000\000\023\000\
\000\000\019\000\000\000\020\000\000\000\012\000\000\000\000\000\
\021\000\022\000\000\000\000\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\052\000\032\000\000\000\032\000\032\000\032\000\032\000\
\000\000\000\000\032\000\032\000\016\000\016\000\000\000\032\000\
\032\000\016\000\016\000\016\000\016\000\016\000\000\000\016\000\
\000\000\016\000\016\000\016\000\016\000\043\000\043\000\016\000\
\016\000\015\000\015\000\047\000\016\000\016\000\015\000\015\000\
\015\000\015\000\015\000\000\000\015\000\000\000\015\000\015\000\
\015\000\015\000\000\000\000\000\015\000\015\000\014\000\014\000\
\000\000\015\000\015\000\014\000\014\000\014\000\014\000\014\000\
\000\000\014\000\000\000\014\000\014\000\014\000\014\000\044\000\
\044\000\014\000\014\000\000\000\050\000\046\000\014\000\014\000\
\013\000\013\000\013\000\013\000\013\000\000\000\013\000\000\000\
\013\000\013\000\013\000\013\000\045\000\045\000\013\000\013\000\
\000\000\045\000\048\000\013\000\013\000\008\000\008\000\008\000\
\008\000\008\000\000\000\008\000\000\000\008\000\008\000\008\000\
\008\000\049\000\049\000\008\000\008\000\000\000\052\000\049\000\
\008\000\008\000\009\000\009\000\009\000\009\000\009\000\000\000\
\009\000\000\000\009\000\009\000\009\000\009\000\054\000\054\000\
\009\000\009\000\000\000\054\000\054\000\009\000\009\000\010\000\
\010\000\010\000\010\000\010\000\000\000\010\000\000\000\010\000\
\010\000\010\000\010\000\042\000\042\000\010\000\010\000\000\000\
\051\000\000\000\010\000\010\000\011\000\011\000\011\000\011\000\
\011\000\000\000\011\000\000\000\011\000\011\000\011\000\011\000\
\000\000\000\000\011\000\011\000\000\000\000\000\000\000\011\000\
\011\000\012\000\012\000\012\000\012\000\012\000\000\000\012\000\
\000\000\012\000\012\000\012\000\012\000\000\000\000\000\012\000\
\012\000\007\000\007\000\000\000\012\000\012\000\000\000\007\000\
\000\000\007\000\007\000\007\000\007\000\000\000\000\000\007\000\
\007\000\003\000\000\000\000\000\007\000\007\000\003\000\000\000\
\003\000\003\000\003\000\003\000\000\000\000\000\003\000\003\000\
\002\000\000\000\000\000\003\000\003\000\002\000\000\000\002\000\
\002\000\002\000\002\000\000\000\000\000\002\000\002\000\000\000\
\000\000\000\000\002\000\002\000"

let yycheck = "\005\000\
\000\000\013\001\019\000\020\000\000\000\016\000\017\000\003\001\
\018\000\001\001\002\001\003\001\004\001\005\001\006\001\027\001\
\022\000\030\001\000\000\000\000\033\001\000\000\001\001\002\001\
\003\001\017\001\018\001\019\001\013\001\013\001\001\000\002\000\
\024\001\025\001\018\001\013\001\028\001\022\001\017\001\023\001\
\013\001\013\001\000\000\021\001\054\000\018\001\063\000\064\000\
\020\001\060\000\061\000\062\000\069\000\018\001\071\000\072\000\
\032\001\074\000\023\001\065\000\031\001\000\000\017\001\000\000\
\003\001\001\001\002\001\003\001\004\001\005\001\006\001\058\000\
\059\000\013\001\012\001\055\000\056\000\057\000\018\001\096\000\
\017\001\017\001\000\000\019\001\000\000\102\000\018\001\014\001\
\024\001\025\001\001\001\023\001\028\001\001\001\002\001\003\001\
\004\001\005\001\026\001\079\000\026\001\081\000\018\001\000\000\
\029\001\000\000\014\001\023\001\023\001\017\001\013\001\019\001\
\014\001\015\001\016\001\027\001\024\001\025\001\007\001\008\001\
\028\001\009\001\010\001\011\001\000\000\001\001\000\000\018\001\
\001\001\002\001\003\001\004\001\005\001\040\000\041\000\052\000\
\255\255\107\000\255\255\075\000\255\255\014\001\255\255\255\255\
\017\001\000\000\019\001\000\000\255\255\255\255\255\255\024\001\
\025\001\255\255\255\255\028\001\001\001\002\001\003\001\004\001\
\005\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\001\001\002\001\003\001\017\001\255\255\019\001\255\255\
\255\255\255\255\255\255\024\001\025\001\255\255\255\255\028\001\
\255\255\017\001\255\255\019\001\255\255\000\000\255\255\255\255\
\024\001\025\001\255\255\255\255\028\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\013\001\018\001\255\255\020\001\021\001\022\001\023\001\
\255\255\255\255\026\001\027\001\007\001\008\001\255\255\031\001\
\032\001\012\001\013\001\014\001\015\001\016\001\255\255\018\001\
\255\255\020\001\021\001\022\001\023\001\026\001\027\001\026\001\
\027\001\007\001\008\001\032\001\031\001\032\001\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\255\255\255\255\026\001\027\001\007\001\008\001\
\255\255\031\001\032\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\026\001\
\027\001\026\001\027\001\255\255\031\001\032\001\031\001\032\001\
\012\001\013\001\014\001\015\001\016\001\255\255\018\001\255\255\
\020\001\021\001\022\001\023\001\026\001\027\001\026\001\027\001\
\255\255\031\001\032\001\031\001\032\001\012\001\013\001\014\001\
\015\001\016\001\255\255\018\001\255\255\020\001\021\001\022\001\
\023\001\026\001\027\001\026\001\027\001\255\255\031\001\032\001\
\031\001\032\001\012\001\013\001\014\001\015\001\016\001\255\255\
\018\001\255\255\020\001\021\001\022\001\023\001\026\001\027\001\
\026\001\027\001\255\255\031\001\032\001\031\001\032\001\012\001\
\013\001\014\001\015\001\016\001\255\255\018\001\255\255\020\001\
\021\001\022\001\023\001\026\001\027\001\026\001\027\001\255\255\
\031\001\255\255\031\001\032\001\012\001\013\001\014\001\015\001\
\016\001\255\255\018\001\255\255\020\001\021\001\022\001\023\001\
\255\255\255\255\026\001\027\001\255\255\255\255\255\255\031\001\
\032\001\012\001\013\001\014\001\015\001\016\001\255\255\018\001\
\255\255\020\001\021\001\022\001\023\001\255\255\255\255\026\001\
\027\001\012\001\013\001\255\255\031\001\032\001\255\255\018\001\
\255\255\020\001\021\001\022\001\023\001\255\255\255\255\026\001\
\027\001\013\001\255\255\255\255\031\001\032\001\018\001\255\255\
\020\001\021\001\022\001\023\001\255\255\255\255\026\001\027\001\
\013\001\255\255\255\255\031\001\032\001\018\001\255\255\020\001\
\021\001\022\001\023\001\255\255\255\255\026\001\027\001\255\255\
\255\255\255\255\031\001\032\001"

let yynames_const = "\
  ABS\000\
  TILDA\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  REM\000\
  CONJ\000\
  DISJ\000\
  EQ\000\
  GT\000\
  LT\000\
  LP\000\
  RP\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  COMMA\000\
  PROJ\000\
  LET\000\
  IN\000\
  END\000\
  BACKSLASH\000\
  DOT\000\
  DEF\000\
  SEMICOLON\000\
  PARALLEL\000\
  LOCAL\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'or_expression) in
    Obj.repr(
# 27 "a3.mly"
                                 ( _1 )
# 343 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'or_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_expression) in
    Obj.repr(
# 31 "a3.mly"
                                           (Disjunction(_1,_3))
# 351 "a3.ml"
               : 'or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_expression) in
    Obj.repr(
# 32 "a3.mly"
                     (_1)
# 358 "a3.ml"
               : 'or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'not_expression) in
    Obj.repr(
# 36 "a3.mly"
                                            (Conjunction(_1,_3))
# 366 "a3.ml"
               : 'and_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'not_expression) in
    Obj.repr(
# 37 "a3.mly"
                     (_1)
# 373 "a3.ml"
               : 'and_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'not_expression) in
    Obj.repr(
# 41 "a3.mly"
                             (Not(_2))
# 380 "a3.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'comparison) in
    Obj.repr(
# 42 "a3.mly"
                 (_1)
# 387 "a3.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comparison) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sub_expression) in
    Obj.repr(
# 46 "a3.mly"
                                   (Equals(_1,_3))
# 395 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comparison) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sub_expression) in
    Obj.repr(
# 47 "a3.mly"
                                   (GreaterT(_1,_3))
# 403 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comparison) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sub_expression) in
    Obj.repr(
# 48 "a3.mly"
                                   (LessT(_1,_3))
# 411 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'comparison) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'sub_expression) in
    Obj.repr(
# 49 "a3.mly"
                                      (GreaterTE(_1,_4))
# 419 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'comparison) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'sub_expression) in
    Obj.repr(
# 50 "a3.mly"
                                      (LessTE(_1,_4))
# 427 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub_expression) in
    Obj.repr(
# 51 "a3.mly"
                                  (_1)
# 434 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sub_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expression) in
    Obj.repr(
# 55 "a3.mly"
                                          ( Sub(_1,_3) )
# 442 "a3.ml"
               : 'sub_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sub_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expression) in
    Obj.repr(
# 56 "a3.mly"
                                         ( Add(_1,_3) )
# 450 "a3.ml"
               : 'sub_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expression) in
    Obj.repr(
# 57 "a3.mly"
                                         (_1)
# 457 "a3.ml"
               : 'sub_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 62 "a3.mly"
                                                 ( Mult(_1,_3) )
# 465 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 63 "a3.mly"
                                               (Div(_1,_3))
# 473 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 64 "a3.mly"
                                               (Rem(_1,_3) )
# 481 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 65 "a3.mly"
                            (_1)
# 488 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 69 "a3.mly"
                             (Abs(_2))
# 495 "a3.ml"
               : 'unary_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 70 "a3.mly"
                             (Negative(_2))
# 502 "a3.ml"
               : 'unary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 71 "a3.mly"
                         (_1)
# 509 "a3.ml"
               : 'unary_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'or_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'or_expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'or_expression) in
    Obj.repr(
# 75 "a3.mly"
                                                                    (IfThenElse(_2,_4,_6))
# 518 "a3.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 76 "a3.mly"
                                               (Project((_3,_5),_7))
# 527 "a3.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "a3.mly"
            (Tuple(0,[]))
# 533 "a3.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tuple) in
    Obj.repr(
# 78 "a3.mly"
                  ( _2 )
# 540 "a3.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'definition) in
    Obj.repr(
# 79 "a3.mly"
                 ( _1 )
# 547 "a3.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tuple) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 83 "a3.mly"
                                (let Tuple(x,y) = _1 in Tuple(x+1,y@[_3]))
# 555 "a3.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'or_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 84 "a3.mly"
                                          (Tuple(2,[_1;_3]))
# 563 "a3.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'definition_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'or_expression) in
    Obj.repr(
# 88 "a3.mly"
                                                 (Let(_2,_4))
# 571 "a3.ml"
               : 'definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function) in
    Obj.repr(
# 89 "a3.mly"
           ( _1 )
# 578 "a3.ml"
               : 'definition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'parenthesis) in
    Obj.repr(
# 93 "a3.mly"
                               ((FunctionAbstraction(_2,_4)))
# 586 "a3.ml"
               : 'function))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'function) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'or_expression) in
    Obj.repr(
# 94 "a3.mly"
                               (FunctionCall(_1,_3))
# 594 "a3.ml"
               : 'function))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parenthesis) in
    Obj.repr(
# 95 "a3.mly"
              (_1)
# 601 "a3.ml"
               : 'function))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 99 "a3.mly"
                                    (_1)
# 608 "a3.ml"
               : 'parenthesis))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'or_expression) in
    Obj.repr(
# 100 "a3.mly"
                                     (InParen(_2))
# 615 "a3.ml"
               : 'parenthesis))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 104 "a3.mly"
                                      (N(_1))
# 622 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "a3.mly"
                                      (Var(_1))
# 629 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 106 "a3.mly"
                                  ((B(_1)))
# 636 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'definition_expression) in
    Obj.repr(
# 110 "a3.mly"
                            (_1)
# 643 "a3.ml"
               : A1.definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'para_definition) in
    Obj.repr(
# 114 "a3.mly"
                  (_1)
# 650 "a3.ml"
               : 'definition_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'seq_definition) in
    Obj.repr(
# 115 "a3.mly"
                 (_1)
# 657 "a3.ml"
               : 'definition_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expression) in
    Obj.repr(
# 116 "a3.mly"
                    (_1)
# 664 "a3.ml"
               : 'definition_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parallel_definition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expression) in
    Obj.repr(
# 120 "a3.mly"
                                                 (match (_1) with 
                                                | Parallel(l1) -> Parallel(l1@[_3])
                                                | _ -> Parallel([_1;_3]))
# 674 "a3.ml"
               : 'para_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expression) in
    Obj.repr(
# 125 "a3.mly"
                    (Parallel(_1))
# 681 "a3.ml"
               : 'parallel_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'seq_definition) in
    Obj.repr(
# 126 "a3.mly"
                 (_1)
# 688 "a3.ml"
               : 'parallel_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parallel_definition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expression) in
    Obj.repr(
# 127 "a3.mly"
                                                 (let Parallel(l1) = _1 in Parallel(l1@[_3]))
# 696 "a3.ml"
               : 'parallel_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sequence_definition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expression) in
    Obj.repr(
# 131 "a3.mly"
                                                  (match (_1) with 
                                                | Sequence(l1) -> Sequence(l1@[_3])
                                                | _ -> Sequence([_1;_3]))
# 706 "a3.ml"
               : 'seq_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expression) in
    Obj.repr(
# 136 "a3.mly"
                    (Sequence(_1))
# 713 "a3.ml"
               : 'sequence_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'para_definition) in
    Obj.repr(
# 137 "a3.mly"
                  (_1)
# 720 "a3.ml"
               : 'sequence_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sequence_definition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expression) in
    Obj.repr(
# 138 "a3.mly"
                                                  (let Sequence(l1) = _1 in Sequence(l1@[_3]))
# 728 "a3.ml"
               : 'sequence_definition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'definition_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'definition_expression) in
    Obj.repr(
# 142 "a3.mly"
                                                          (Local(_2,_4))
# 736 "a3.ml"
               : 'simple_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 143 "a3.mly"
                         (Simple(_2,_4))
# 744 "a3.ml"
               : 'simple_expression))
(* Entry def_parser *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry exp_parser *)
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
let def_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : A1.definition)
let exp_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : A1.exptree)
