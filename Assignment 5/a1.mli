open A0

(* abstract syntax *)
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
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
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
  | App of exptree * exptree
(* definition *)
and definition =
    Simple of string * exptree
  | SimpleType  of string * exptype * exptree
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition
(* The possible types of expressions in the language of expressions *)
and exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)
(* opcodes of the stack machine (in the same sequence as above) *)
type closure = Closure of (expr*(string -> closure))
(* type of elements in the stack *)
type stk_elements = ARG of closure | PLUS of closure | MULT of closure | AND of closure | OR of closure | IFTE of (closure*closure) |CMP;;
val krivine: closure -> (stk_elements list) -> closure
(* opcodes of SECD *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool 
| CMP | ADD | INTO | AND | OR | COND of (opcode list * opcode list)
| CLOS of (string * opcode list) | RET | APP 
type answer = Num of bigint | BoolVal of bool | Triple of (string * opcode list);;
type answerClosure = Ansclos of (answer * (string -> answerClosure));;
type dumpType = Dump of (answerClosure list * (string -> answerClosure) * opcode list);;
val secdmc: (answerClosure list) -> (string -> answerClosure) -> (opcode list) -> (dumpType list) -> answerClosure
val compile: expr -> (opcode list)
