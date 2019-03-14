(* Header *)
{
	open A3
	exception InvalidToken of char;;
}
(* Regex for lower case expression *)
let letterUpper = ['A'-'Z']
(* Regex for letter *)
let letter = ['a'-'z' 'A'-'Z' '_' '\'']
let digit = ['0'-'9']
let digit_ = ['1'-'9']
(* regex for integer with no leading zeros *)
(* let integer = ['+' '-']?('0'|((digit_)digit *)
(* regex for identifier starting with a lower case letter*)
let identifiers = (letterUpper)(letter|digit)*
(* Regex for whitespace, newline is also considerred in whitespace *)
let whitespace = [' ' '\t' '\n']+

rule read = parse
	eof {(EOF)}
| digit+ as i {(INT(int_of_string i))}
| ','		{(COMMA)}
| '~'    {(TILDA)}
| "proj"	{(PROJ)}
| "abs" {(ABS)}
| "+"  {(PLUS)}
| "-"  {(MINUS)}
| "*"  {(TIMES)}
| "div" {(DIV)}
| "mod"  {(REM)}
| '(' {(LP)}
| ')' {(RP)}
| 'T' {(BOOL(true))}
| 'F' {(BOOL(false))}	
|"not" {(NOT)}
| "/\\" {(CONJ)}
| "\\/" {(DISJ)}
| "=" {(EQ)}
| ">" {(GT)}
| "<" {(LT)}
| "if" {(IF)}
| "else" {(ELSE)}
| "then" {(THEN)}
| "fi"		{(FI)}
| identifiers as i {(ID(i))}
| whitespace {read lexbuf}
| _ as s {raise (InvalidToken s)}(* An exception is raised if any string apart from expected strings are encountered *)

