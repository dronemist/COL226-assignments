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
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF LP RP
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/
main:
    expression EOF            { $1 }          /* $n on the rhs returns the value for nth symbol in the grammar on lhs */
;

expression:
    comparision { $1 }
    | LP RP {Tuple(0,[])}
    | LP tuple RP { $2 }
    | PROJ LP INT COMMA INT RP expression      {Project(($3,$5),$7)} 
    | IF expression THEN expression ELSE expression FI     {IfThenElse($2,$4,$6)}
;

tuple:
    | expression COMMA tuple {let Tuple(x,y) = $3 in Tuple(x+1,$1::y)} 
    | expression              {Tuple(1,[$1])}
;
comparision:
    sub_expression              {$1}
    | comparision EQ sub_expression {Equals($1,$3)}
    | comparision GT sub_expression {GreaterT($1,$3)}
    | comparision LT sub_expression {LessT($1,$3)}
    | comparision GT EQ sub_expression {GreaterTE($1,$4)}
    | comparision LT EQ sub_expression {LessTE($1,$4)}
;

sub_expression:
    sub_expression MINUS mult_expression { Sub($1,$3) }
    |sub_expression PLUS mult_expression { Add($1,$3) } 
    |sub_expression DISJ mult_expression    {Disjunction($1,$3)}
    |mult_expression                    {$1}
;

mult_expression:
    mult_expression TIMES unary_expression     { Mult($1,$3) }
    | mult_expression DIV unary_expression     {Div($1,$3)}
    | mult_expression REM unary_expression     {Rem($1,$3) }
    | unary_expression      {$1}
;
unary_expression:
    parenthesis         {$1}
    | ABS unary_expression   {Abs($2)}
    | TILDA unary_expression  {Negative($2)}
    | NOT unary_expression     {Not($2)}
;
parenthesis:
    constant                        {$1}
    | LP expression RP            {InParen($2)}
;
constant:
    INT                               {N($1)}
    |ID                               {Var($1)}
    |BOOL                         {(B($1))}
;