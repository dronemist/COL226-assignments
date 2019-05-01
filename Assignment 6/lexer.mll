(* Header *)
{
	open Parser
	exception InvalidToken of char;;
}
(* Regex for lower case expression *)
let letterUpper = ['a'-'z' | 'A'-'Z']
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
| ':'		{(COLON)}
| '='    {(EQ)}
| "("	{(LP)}
| ")" {(RP)}
| ',' {COMMA}
| 'T' {(BOOL(true))}
| 'F' {(BOOL(false))}
| "RET" {RETURN}
| identifiers as i {(ID(i))}
| _ as s {raise (InvalidToken s)}