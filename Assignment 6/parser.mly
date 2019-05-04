%{
    open A6                       
%}
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token LP RP COLON EQ RETURN EOF COMMA STACK PROCEDURES STATIC VARIABLES EOL OPTIONS
%start act_parser
%type <A6.actions> act_parser 
%%
act_parser:
    basic_actions EOL {$1}
;

basic_actions:
    |OPTIONS{PrintOptions}
    |STACK {PrintStk}
    |PROCEDURES {PrintProc}
    |STATIC {PrintSL}
    |VARIABLES {PrintVar}
    | expression {$1}

expression:
    function_call {$1}
    ;
function_call:
    ID LP expr_list RP {FunctionCall($1,$3)} 
    | assign {$1}
    ;
expr_list:
    | basic COMMA expr_list {$1::$3}
    | basic {[$1]}
    ;
assign:
    return {$1}
    | ID COLON EQ basic {Command($1,$4)} 
    ;
return:
    RETURN {Return}
    ;
basic:
    INT                               {Integer($1)}
    |ID                               {V($1)}
    |BOOL                         {(Bool($1))}
    ;