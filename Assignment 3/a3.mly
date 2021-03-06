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
    | expression         {$1}
;
/* I have kept the precedence of proj tuple and if then else as the same as I feel this is how it is supposed to be */
expression:
    | IF or_expression THEN or_expression ELSE or_expression FI     {IfThenElse($2,$4,$6)} 
    | PROJ LP INT COMMA INT RP expression      {Project(($3,$5),$7)}
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