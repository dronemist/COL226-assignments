%{
    open A1
    let mktpl t1 t2 = match t2 with 
                    | Tuple(x,l) -> (match t1 with
                                    | Done -> Tuple(x,l)
                                    | _ -> Tuple(x+1,t1::l)
                                    )
                    | Done ->       (match t1 with
                                    | Done -> Done
                                    | _ -> Tuple(1,[t1])
                                    )
                    | _ ->          (match t1 with
                                    | Done -> Tuple(1,[t2])
                                    | _ -> Tuple(2,t1::[t2])
                                    )                                     
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
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/
main:
    if_then_else              { $1 }          /* $n on the rhs returns the value for nth symbol in the grammar on lhs */
    | EOF                              { Done }
;
if_then_else:
    | IF expression THEN if_then_else ELSE if_then_else FI     {IfThenElse($2,$4,$6)}
    | expression   { $1 }
;
expression:
    tuple { $1 }
    | LP tuple RP { $2 }
;
tuple:
    | expression COMMA tuple {(mktpl $1 $3)} 
    | comparision               {$1}
;
comparision:
    sub_expression              {$1}
    | sub_expression EQ comparision {Equals($1,$3)}
    | sub_expression GT comparision {GreaterT($1,$3)}
    | sub_expression LT comparision {LessT($1,$3)}
    | sub_expression GT EQ comparision {GreaterTE($1,$4)}
    | sub_expression LT EQ comparision {LessTE($1,$4)}
;

sub_expression:
    add_expression MINUS sub_expression { Sub($1,$3) }
    |add_expression DISJ sub_expression    {Disjunction($1,$3)}
    |add_expression                     {$1}
;
add_expression:
     mult_expression PLUS add_expression { Add($1,$3) } /* Created a tree with PLUS at root and two subtrees corresponding to left: add_expression and right: mult_expression */
    | mult_expression CONJ add_expression {Conjunction($1,$3)}
    | mult_expression                    { $1 }
;
mult_expression:
    div_expression TIMES mult_expression     { Mult($1,$3) }
    | NOT mult_expression                   {Not($2)}
    | div_expression                       { $1 }
;
div_expression:
    mod_expression DIV div_expression       {Div($1,$3)}
    |mod_expression                         {$1}
;
mod_expression:
    unary_expression REM mod_expression       {Rem($1,$3)}
    | unary_expression                      {$1}
;
unary_expression:
    parenthesis         {$1}
    | ABS unary_expression   {Abs($2)}
    | TILDA unary_expression  {Negative($2)}
;
parenthesis:
    constant                        {$1}
    | LP if_then_else RP            {$2}
;
constant:
    INT                               {N($1)}
    |ID                               {Var($1)}
    |BOOL                         {(B($1))}
;