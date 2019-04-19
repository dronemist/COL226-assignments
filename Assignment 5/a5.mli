(* the definitional interpreter *)
open A0
exception Not_implemented;;
type  exptree =
  V of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | Integer of int      (* Integer constant *)
  | Bool of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Plus of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | And of exptree * exptree (* conjunction /\ *)
  | Or of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  | Cmp of exptree
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | If_Then_Else of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)
  | Let of definition * exptree
  | Lambda of string * exptype * exptree
  | RecLambda of string * string * exptype * exptree
  | App of exptree * exptree
and exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

and definition =
    Simple of string * exptree
  (* | RecLambda of (string * exptree)  *)
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition;;
  (* opcode list *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS 
| CMP | ADD | INTO | AND | OR | COND of (opcode list * opcode list) | DIV | REM | EQS | GTE | LTE | GT | LT
| PAREN | NOT | CLOS of (string * opcode list)| RCLOS of (string * string * opcode list) | RET | APP | TUPLE of int | PROJ of int*int | SUB | SIMPLEDEF of string | LET | RECDEF of string;;
type answer = Num of bigint | BoolVal of bool | Triple of (string * opcode list) | Rectriple of (string * string * opcode list)  | Tup of int * (answer list) ;;
type closure = Closure of (exptree*((string * closure) list))|VClosure of (answer*((string * closure) list));;
type answerClosure = Ansclos of (answer * ((string * answerClosure) list));;
type dumpType = Dump of (answerClosure list * ((string * answerClosure) list) * opcode list);;
(* type of elements in the stack of krivine *)
type stk_elements = ARG of closure | PLUS_stk of closure | MULT of closure | AND of closure | OR of closure | IFTE of (closure*closure) |CMP_stk | SUB of closure 
| GT of closure | GTE of closure| LT of closure|LTE of closure|EQ of closure;;val krivine: closure -> (stk_elements list) -> (closure)
val secdmc: (answerClosure list) -> ((string * answerClosure) list) -> (opcode list) -> (dumpType list) -> answerClosure
val compile: exptree -> (opcode list)
val ansSECD: exptree -> string