# Building a scanner using OCaml-Lex
## Installation 
Make sure utop is installed, keep a2.mll .ocamlinit and compile.sh in the same folder then type the following command on the terminal
```
bash compile.sh
```
## Expressions
The expressions include:
- Integer constants with an optional sign
- Binary arithmetic operators:
  - **\+** (Addition)
  - **\-** (Subtraction)
  - **\*** (Multiplication)
  - **div** (Division)
  - **^** (Exponentiation)
- Unary arithmetic operators:
  - **abs** (Absolute value)
- Parenthesis:
  - **(**
  - **)**
- Boolean constants:
  - **T** (True)
  - **F** (False)
- Unary boolean operation:
  - **not**
- Binary boolean operations:  
  - **\/\\**  (and)
  - **\\/**  (or)
- Comparison operators:
  - **=**  (equal)
  - **>**  (greater than)
  - **<**  (less than)
  - **>=**  (greater or equal)
  - **<=**  (less or equal)
- A conditional operator consisting of three tokens:
  - **if then else**
- _Identifiers_ which are alphanumeric strings beginning with lower-case letter.
- A definition construct:
  - **def**
- _A delimiter_ to terminate the expression:
  - **;**
## Lexer
The lexer was implemented in order to identify the following tokens:
```ocaml
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
```
To lex a string use the following command `scanner string_to_be_lexed` and the function scanner would return a list of tokens. If an invalid string is entered an exception is raised.
