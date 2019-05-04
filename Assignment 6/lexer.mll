(* Header *)
{
	open Parser
	exception InvalidToken of char;;
	exception Exit;;
}
(* Regex for lower case expression *)
let letterUpper = ['a'-'z' 'A'-'Z']
(* Regex for letter *)
let letter = ['a'-'z' 'A'-'Z' '_' '\'']
let digit = ['0'-'9']
let digit_ = ['1'-'9']
(* regex for integer with no leading zeros *)
(* let integer = ['+' '-']?('0'|((digit_)digit *)
(* regex for identifier starting with a upper case letter*)
let identifiers = (letterUpper)(letter|digit)*
(* Regex for whitespace, newline is also considerred in whitespace *)
let whitespace = [' ' '\t' '\n']+
rule read = parse
	eof {(EOF)}
| digit+ as i {(INT(int_of_string i))}
| "#0" {OPTIONS}
| "#1" {STACK}
| "#2" {PROCEDURES}
| "#3" {VARIABLES}
| "#4" {STATIC}
| ':'		{(COLON)}
| "="    {(EQ)}
| "("	{(LP)}
| ")" {(RP)}
| ',' {COMMA}
| ';' {EOL}
| "True" {(BOOL(true))}
| "False" {(BOOL(false))}
| "RET" {RETURN}
| "exit()" {raise Exit}
| identifiers as i {(ID(i))}
| whitespace {read lexbuf}
| _ as s {raise (InvalidToken s)}