%{
    open A6                       
%}
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token LP RP COLON EQ RETURN EOF COMMA
%start act_parser
%type <A6.actions> act_parser 
act_parser:
    expression EOF {$1}
;
expression:
    function_call {$1}
    ;
function_call:
    ID LP expr_list RP {FunctionCall($1,$3)} 
    | assign {$1}
    ;
expr_list:
| expression COMMA expr_list {$1::$3}
| expression {[$1]}
;
assign:
return {$1}
| ID COLON EQ expression {Comman($1,$4)} 
;
return:
RETURN {Return}
| basic {$1}
;
basic:
INT                               {Integer($1)}
|ID                               {V($1)}
|BOOL                         {(Bool($1))}
;