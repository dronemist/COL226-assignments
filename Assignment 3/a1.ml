(* Dummy implementation of A1 *)
open A0
exception NtupleNotPossible;;
exception IthProjNotPossible;;
exception Not_implemented;;
type answer = Num of bigint | Bool of bool | T of answer*answer;;

type  exptree = Done
  | N of int (* Integer constant *)
  | B of bool (* Boolean constant *)
  | Var of string (* variable *)
  | Conjunction of exptree * exptree (* binary operators on booleans /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  | Equals of exptree * exptree      (* comparison operations on integers *)
  | GreaterTE of exptree * exptree   (* comparison operations on integers *)
  | LessTE of exptree * exptree      (* comparison operations on integers *)
  | GreaterT of exptree * exptree    (* comparison operations on integers *)
  | LessT of exptree * exptree       (* comparison operations on integers *)
  | InParen of exptree               (* expressions using parenthesis *)
  | IfThenElse of exptree * exptree * exptree (* a conditional expression *)
  | Tuple of int * (exptree list)           (* creating n-tuples (n >= 0) *)
  | Project of (int*int) * (exptree list)          (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Plus of exptree * exptree        (* binary operators on integers *)
  | Minus of exptree * exptree       (* binary operators on integers *)
  | Mult of exptree * exptree        (* binary operators on integers *)
  | Div of exptree * exptree         (* binary operators on integers *)
  | Rem of exptree * exptree         (* binary operators on integers *)
  | Nega of exptree       (* unary operators on booleans *)
  | Abs of exptree        (* unary operators on integers *)
;;
type opcode = NCONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS
  | EQS | GTE | LTE | GT | LT | PAREN
  | BCONST of bool | CONJ | DISJ | NOT
  | IFTE | TUPLE of int | PROJ of int
;;
(* This function extracts the bigint from answer type *)
let bigint_of_Num t = match t with 
| Num(x) -> x
| _ -> (mk_big 0);;

(* This function extracts bool from answer type *)
let bool_of_Bool b = match b with
| Bool(x) -> x
| _ -> true;;
(* Map from string variable names to their values. New values can be added *)
module VarTable = Map.Make(String)

(* evaluating the tree where is th hashtable containing values *)
let rec eval t = match t with
    | N(x) -> (Num (mk_big x))
    | B(x) -> (Bool x)
    | Var(x) -> Num(mk_big 4)
    | Conjunction(t1,t2) -> Bool(bool_of_Bool(eval t1) && bool_of_Bool(eval t2))
    | Disjunction(t1,t2) -> Bool(bool_of_Bool(eval t1) || bool_of_Bool(eval t2))
    | Equals(t1,t2) -> Bool(eq (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | GreaterTE(t1,t2) -> Bool(geq (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | LessTE(t1,t2) -> Bool(leq (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | GreaterT(t1,t2) -> Bool(gt (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | LessT(t1,t2) -> Bool(lt (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | InParen(t1) -> (eval t1)
    | IfThenElse(t1,t2,t3) -> if bool_of_Bool(eval t1) then (eval t2)
                              else (eval t3)
    | Tuple(x,l1) -> if x != (List.length l1) then raise NtupleNotPossible 
                     else 
                      (
                        match l1 with 
                        | [y] -> (eval y)
                        | y::ys -> T((eval y),eval(Tuple(x-1,ys)))
                      )
    | Project((i,x),l1) -> if i > x then raise IthProjNotPossible
                        else if i == 1 then (eval (List.hd l1))
                        else (eval (Project((i-1,x-1),(List.tl l1))))                    
    | Plus(t1,t2) -> Num(add (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | Minus(t1,t2) -> Num(sub (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | Mult(t1,t2) -> Num(mult (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | Div(t1,t2) -> Num(div (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | Rem(t1,t2) -> Num(rem (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | Nega(t1) -> Num(minus (bigint_of_Num(eval t1)))
    | Abs(t1) -> Num(abs (bigint_of_Num(eval t1)))
;;
let stackmc stk pgm = raise Not_implemented;;
let compile ex = raise Not_implemented;;
