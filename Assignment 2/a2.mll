(* Header *)
{
	type token =
	   INT of int          (* integer constant, positive or negative w/o leading zeros *)
	|  TRUE                (* boolean constant "T" *)
	|  FALSE               (* boolean constant "F" *)
	|  ABS                 (* unary operator, "abs" *)
	|  PLUS                (* arithmetic plus, "+" *)
	|  MINUS               (* arithmetic minus, "-" *)
	|  MUL                 (* arithmetic multiply, "*" *)
	|  DIV                 (* integer div, "div" *)
	|  MOD                 (* remainder, "mod" *)
	|  EXP                 (* exponentiation, "^" *)
	|  LP                  (* left paren, "(" *)
	|  RP                  (* right paren, ")" *)
	|  NOT                 (* boolean NOT, "not" *)
	|  AND                 (* boolean AND, "/\ " *)
	|  OR                  (* boolean OR, "\/" *)
	|  EQ                  (* equal to, "=" *)
	|  GTA                 (* greater than, ">" *)
	|  LTA                 (* less than, "<" *)
	|  GEQ                 (* greater than/equal to, ">=" *)
	|  LEQ                 (* less than/equal to, "<=" *)
	|  IF                  (* keyword "if" *)
	|  THEN                (* keyword "then" *)
	|  ELSE                (* keyword "else" *)
	|  ID of string        (* variable identifier, alphanumeric string with first char lowercase *)
	|  DEF                 (* definition construct, "def" *)
	|  DELIMITER;; 		   (* delimiter, ";" *)
	exception InvalidToken of char;;
}
(* Regex for lower case expression *)
let letterLower = ['a'-'z']
(* Regex for letter *)
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let digit_ = ['1'-'9']
(* regex for integer with no leading zeros *)
let integer = ['+' '-']?('0'|((digit_)digit*))
(* regex for identifier starting with a lower case letter*)
let identifiers = (letterLower)(letter|digit)*
(* Regex for whitespace, newline is also considerred in whitespace *)
let whitespace = [' ' '\t' '\n']+
(* Assumed  *)
rule read = parse
	eof {[]}
| integer as i { 
	if i.[0] = '+' then
	let new_int = String.sub (i) (1) ((String.length i)-1) in
	(INT(int_of_string new_int))::(read lexbuf)
	else
	(INT(int_of_string i))::(read lexbuf)
}
| "abs" {(ABS)::(read lexbuf)}
| "+"  {(PLUS)::(read lexbuf)}
| "-"  {(MINUS)::(read lexbuf)}
| "*"  {(MUL)::(read lexbuf)}
| "div" {(DIV)::(read lexbuf)}
| "mod"  {(MOD)::(read lexbuf)}
| "^"  {(EXP)::(read lexbuf)}
| '(' {(LP)::(read lexbuf)}
| ')' {(RP)::(read lexbuf)}
| 'T' {(TRUE)::(read lexbuf)}
| 'F' {(FALSE)::(read lexbuf)}	
|"not" {(NOT)::(read lexbuf)}
| "/\\" {(AND)::(read lexbuf)}
| "\\/" {(OR)::(read lexbuf)}
| "=" {(EQ)::(read lexbuf)}
| ">" {(GTA)::(read lexbuf)}
| "<" {(LTA)::(read lexbuf)}
| ">=" {(GEQ)::(read lexbuf)}
| "<=" {(LEQ)::(read lexbuf)}
| "if" {(IF)::(read lexbuf)}
| "else" {(ELSE)::(read lexbuf)}
| "then" {(THEN)::(read lexbuf)}
| "def" {(DEF)::(read lexbuf)}
| identifiers as i {(ID(i))::(read lexbuf)}
| ";" {(DELIMITER)::(read lexbuf)}
| whitespace {(read lexbuf)}
| _ as s {raise (InvalidToken s)}(* An exception is raised if any string apart from expected strings are encountered *)

(* trailer *)
{
	let scanner s = read(Lexing.from_string s)
}
(*
******************************* Examples *******************************************

	scanner "if i = 2 then i = i + 1 else i = i - 1; ";;                                               
	[IF; ID "i"; EQ; INT 2; THEN; ID "i"; EQ; ID "i"; PLUS; INT 1; ELSE;             
	ID "i"; EQ; ID "i"; MINUS; INT 1; DELIMITER] 
	
	scanner "TF Rest";;
	Exception: InvalidInput 'R'.

	scanner "if(i >= 2) then i = i + 1 else if(i<= -2) then  i = i - 1; ";;
	[IF; LP; ID "i"; GEQ; INT 2; RP; THEN; ID "i"; EQ; ID "i"; PLUS; INT 1; ELSE; 
	IF; LP; ID "i"; LEQ; INT (-2); RP; THEN; ID "i"; EQ; ID "i"; MINUS; INT 1; DELIMITER]

	scanner "a b T F ()() def abs aSSFJNjdksn + * - div ";;
	[ID "a"; ID "b"; TRUE; FALSE; LP; RP; LP; RP; DEF; ABS; ID "aSSFJNjdksn"; PLUS;  MUL; MINUS; DIV] 

	scanner "T F * mod div";;
	[TRUE; FALSE; MUL; MOD; DIV] 

	scanner "not \\/ /\\";;
	[NOT; OR; AND]
	
	scanner "not \\/ /\\ abHJVHGF T awqwe F also";;
	[NOT; OR; AND; ID "abHJVHGF"; TRUE; ID "awqwe"; FALSE; ID "also"]

	scanner "ifx=5thenx=9;";;
	[ID "ifx"; EQ; INT 5; ID "thenx"; EQ; INT 9; DELIMITER]	

	scanner "This ^ ><= =";;
	[TRUE; ID "his"; EXP; GTA; LEQ; EQ]

	scanner "52-5 ";;
	[INT 52; INT (-5)] 

	scanner "52 - 5 ";;
	token list = [INT 52; MINUS; INT 5]


****************************** Counter Examples ************************************
	
	scanner "def3";;
	[ID "def3"] 

	scanner "def 3";;
	[DEF; INT 3]	
	
	scanner "5#b";;
	Exception: InvalidToken '#'.
	
	scanner " &&    ";;
	Exception: InvalidToken '&'.

	scanner " %$    ";;
	Exception: InvalidToken '%'.
*)