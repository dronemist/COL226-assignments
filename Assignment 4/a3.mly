%{
    open A1                            
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ
LET IN END BACKSLASH DOT DEF SEMICOLON COLON ARROW TINT TUNIT TBOOL PARALLEL LOCAL EOF
%start def_parser exp_parser type_parser
%type <A1.definition> def_parser /* Returns definitions */
%type <A1.exptree> exp_parser /* Returns expression */
%type <A1.exptype> type_parser /* Returns type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/
exp_parser:
    or_expression EOF            { $1 }          /* $n on the rhs returns the value for nth symbol in the grammar on lhs */
;

or_expression:
    | or_expression DISJ and_expression    {Disjunction($1,$3)}
    | and_expression {$1}
;

and_expression:
    | and_expression CONJ not_expression    {Conjunction($1,$3)} 
    | not_expression {$1} 
;

not_expression:
    | NOT not_expression     {Not($2)}
    | comparison {$1}   
;

comparison:
    | comparison EQ sub_expression {Equals($1,$3)}
    | comparison GT sub_expression {GreaterT($1,$3)}
    | comparison LT sub_expression {LessT($1,$3)}
    | comparison GT EQ sub_expression {GreaterTE($1,$4)}
    | comparison LT EQ sub_expression {LessTE($1,$4)}
    | sub_expression              {$1}
;

sub_expression:
    |sub_expression MINUS mult_expression { Sub($1,$3) }
    |sub_expression PLUS mult_expression { Add($1,$3) }
    | mult_expression                    {$1} 
       
;

mult_expression:
    | mult_expression TIMES unary_expression     { Mult($1,$3) }
    | mult_expression DIV unary_expression     {Div($1,$3)}
    | mult_expression REM unary_expression     {Rem($1,$3) }  
    | unary_expression      {$1}
;

unary_expression:
    | ABS unary_expression   {Abs($2)}
    | TILDA unary_expression {Negative($2)}
    | if_then_else        {$1}
;

if_then_else:
    | IF or_expression THEN or_expression ELSE or_expression FI     {IfThenElse($2,$4,$6)}
    | PROJ LP INT COMMA INT RP if_then_else      {Project(($3,$5),$7)}
    | function_call_one {$1}
;

function_call_one:
    | definition {$1}
    | function_call_one LP or_expression RP {FunctionCall($1,$3)}
    | BACKSLASH ID COLON type_parser DOT definition {(FunctionAbstraction($2,$4,$6))} 
;

definition:
| LET definition_expression IN or_expression END {Let($2,$4)}
| expression { $1 }
;

expression:
    | LP RP {Tuple(0,[])}
    | LP tuple RP { $2 } 
    | parenthesis { $1 }
;
/* To generate the expression list */
tuple:
    | tuple COMMA or_expression {let Tuple(x,y) = $1 in Tuple(x+1,y@[$3])} 
    | or_expression COMMA or_expression   {Tuple(2,[$1;$3])}
;

parenthesis:
    constant                        {$1}
    | LP or_expression RP            {InParen($2)}
;

constant:
    INT                               {N($1)}
    |ID                               {Var($1)}
    |BOOL                         {(B($1))}
;

def_parser:
  definition_expression EOF {$1} 
;

definition_expression:
| para_definition {$1}
| seq_definition {$1}
| simple_expression {$1}
;

para_definition:
| parallel_definition PARALLEL simple_expression {match ($1) with 
                                                | Parallel(l1) -> Parallel(l1@[$3])
                                                | _ -> Parallel([$1;$3])}
;
parallel_definition:
| simple_expression {Parallel([$1])}
| seq_definition {$1}
| parallel_definition PARALLEL simple_expression {let Parallel(l1) = $1 in Parallel(l1@[$3])}
;

seq_definition:
| sequence_definition SEMICOLON simple_expression {match ($1) with 
                                                | Sequence(l1) -> Sequence(l1@[$3])
                                                | _ -> Sequence([$1;$3])}
;
sequence_definition:
| simple_expression {Sequence([$1])}
| para_definition {$1}
| sequence_definition SEMICOLON simple_expression {let Sequence(l1) = $1 in Sequence(l1@[$3])}
;

simple_expression:
|LOCAL definition_expression IN definition_expression END {Local($2,$4)}
|DEF ID EQ or_expression {Simple($2,$4)}
|DEF ID COLON type_parser EQ or_expression {SimpleType($2,$4,$6)}
;


type_parser:
| function_type    {$1}
;

function_type:
| function_type ARROW other_types {Tfunc($1,$3)}
| other_types {$1}
;
other_types:
| LP type_list RP {$2}
| paren {$1}
;

type_list:
| type_list TIMES type_parser {let Ttuple(l1) = $1 in Ttuple(l1@[$3])}
| type_parser TIMES type_parser  {Ttuple($1::[$3])}
;

paren:
|LP type_parser RP {$2}
| simple_type {$1}
;

simple_type:
TINT {Tint}
|TBOOL {Tbool}
|TUNIT  {Tunit}
;

/* type_parser:
| function_type    {$1}
;
function_type:
| tuple_type ARROW function_type  {Tfunc($1,$3)}
| tuple_type {$1}
;
tuple_type:
| paren TIMES tuple_type  {match $3 with Ttuple(l1) ->  Ttuple($1::l1)
                            | _ -> Ttuple[$1;$3]}
| paren {$1}
;

paren:
|LP type_parser RP {$2}
| simple_type {$1}
;

simple_type:
|TINT {Tint}
|TBOOL {Tbool}
|TUNIT  {Tunit}
; */
