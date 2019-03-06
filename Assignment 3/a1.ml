(* Dummy implementation of A1 *)
open A0
exception NtupleNotPossible;;
exception IthProjNotPossible;;
exception InvalidProjection;;
exception Not_implemented;;
exception BadStack;;
type  exptree =  Done (* End of input *)
  | Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
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
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)

(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

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

(* evaluating the tree where is the hashtable containing values *)
let rec eval t = match t with
    | N(x) -> (Num (mk_big x))
    | B(x) -> (Bool x)
    | Var(x) -> Num(mk_big 4)
    | Conjunction(t1,t2) -> Bool(bool_of_Bool(eval t1) && bool_of_Bool(eval t2))
    | Disjunction(t1,t2) -> Bool(bool_of_Bool(eval t1) || bool_of_Bool(eval t2))
    | Not(t1) ->  Bool(not (bool_of_Bool(eval t1)))
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
                        | [y] -> Tup(x,[eval y])
                        | y::ys -> let Tup(a,list) = eval(Tuple(x-1,ys)) in
                                      Tup(x,(eval y)::list)
                      )
    | Project((i,x),t1) -> if i > x then raise IthProjNotPossible
                        else 
                        (match t1 with
                        | Tuple(a,list) -> if i != x then raise InvalidProjection
                                           else 
                                          (  if i == 1 then (eval (List.hd list))
                                            else (eval(Project((i-1,x-1),Tuple(a-1,(List.tl list))))) 
                                          )
                        | _ -> raise IthProjNotPossible
                        )                      
    | Add(t1,t2) -> Num(add (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | Sub(t1,t2) -> Num(sub (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | Mult(t1,t2) -> Num(mult (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | Div(t1,t2) -> Num(div (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | Rem(t1,t2) -> Num(rem (bigint_of_Num(eval t1)) (bigint_of_Num(eval t2)))
    | Negative(t1) -> Num(minus (bigint_of_Num(eval t1)))
    | Abs(t1) -> Num(abs (bigint_of_Num(eval t1)))
;;
(* stackmc evaluated the opcode list *)
let rec stackmc stk pgm = match pgm with
    | [] -> (match stk with
            | [] -> raise BadStack
            | x::xs -> x
    )
    | x::xs -> ( match x with
                | NCONST (y) -> stackmc ((Num(y))::stk) (xs)
                | BCONST (y) -> stackmc ((Bool(y))::stk) (xs)
                | PLUS -> (	(* The list should have atleast two elements if we match a binary operator *)
                      match stk with
                      | z1::z2::zs -> stackmc (Num(add (bigint_of_Num z2) (bigint_of_Num z1))::zs) xs
                      | _ -> raise BadStack
                    )	
                | MINUS ->  (	match stk with
                      | z1::z2::zs -> stackmc (Num(sub (bigint_of_Num z2) (bigint_of_Num z1))::zs) xs
                      | _ -> raise BadStack
                    )
                | MULT ->  (	match stk with
                      | z1::z2::zs -> stackmc (Num(mult (bigint_of_Num z2) (bigint_of_Num z1))::zs) xs
                      | _ -> raise BadStack
                    )
                | DIV ->	(	match stk with
                      | z1::z2::zs -> stackmc (Num(div (bigint_of_Num z2) (bigint_of_Num z1))::zs) xs
                      | _ -> raise BadStack
                    )
                | REM ->	(	match stk with
                      | z1::z2::zs -> stackmc (Num(rem (bigint_of_Num z2) (bigint_of_Num z1))::zs) xs
                      | _ -> raise BadStack
                    ) 
                | UNARYMINUS ->	(	
                      match stk with
                      | z1::zs -> stackmc ((Num(minus (bigint_of_Num z1)))::zs) xs
                      | _ -> raise BadStack
                    )
                | ABS -> (	match stk with
                      | z1::zs -> stackmc ((Num(abs (bigint_of_Num z1)))::zs) xs
                      | _ -> raise BadStack
                    )
                | NOT -> (	match stk with
                      | z1::zs -> stackmc ((Bool(not (bool_of_Bool z1)))::zs) xs
                      | _ -> raise BadStack
                    )    
                | EQS ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool(eq (bigint_of_Num z2) (bigint_of_Num z1)))::zs) xs
                      | _ -> raise BadStack
                    )
                | CONJ ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool((bool_of_Bool z1) && (bool_of_Bool z2)))::zs) xs
                      | _ -> raise BadStack
                    )
                | DISJ ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool((bool_of_Bool z1) || (bool_of_Bool z2)))::zs) xs
                      | _ -> raise BadStack
                    )
                | GT ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool(gt (bigint_of_Num z2) (bigint_of_Num z1)))::zs) xs
                      | _ -> raise BadStack
                    )
                | GTE ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool(geq (bigint_of_Num z2) (bigint_of_Num z1)))::zs) xs
                      | _ -> raise BadStack
                    )
                | LT ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool(lt (bigint_of_Num z2) (bigint_of_Num z1)))::zs) xs
                      | _ -> raise BadStack
                    )
                | LTE ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool(leq (bigint_of_Num z2) (bigint_of_Num z1)))::zs) xs
                      | _ -> raise BadStack
                    )
                | PAREN -> (	match stk with
                      | z1::zs -> stackmc ((z1)::zs) xs
                      | _ -> raise BadStack
                    )
                | IFTE ->	(	match stk with
                      | z1::z2::z3::zs -> if (bool_of_Bool z3) then (stackmc (z2::zs) xs) 
                        else stackmc (z1::zs) xs
                      | _ -> raise BadStack
                    )
                | TUPLE(a) -> if a == 1 then 	
                              (
                                match stk with
                                | z1::z2::zs -> ( match z1 with 
                                          | Tup(b,l1) -> stackmc ((Tup(b+1,z2::l1))::zs) xs
                                          | _ -> stackmc ((Tup(1,[z1]))::z2::zs) xs
                                )
                                | [z1] ->  stackmc ([(Tup(1,[z1]))]) xs	
                                | _ -> raise BadStack
                              )
                              else
                              (
                                match stk with
                                | z1::z2::zs -> ( match z1 with 
                                          | Tup(b,l1) -> stackmc ((Tup(b+1,z2::l1))::zs) (TUPLE(a-1)::xs)
                                          | _ -> stackmc ((Tup(1,[z1]))::z2::zs) (TUPLE(a-1)::xs)
                                )
                                | _ -> raise BadStack 
                              )
                | PROJ(x,y) -> match stk with
                              | Tup(y,l1)::zs ->( if x==1 then stackmc ((List.hd l1)::zs) xs
                                                  else
                                                    stackmc ((Tup(y-1,(List.tl l1)))::zs) (PROJ(x-1,y-1)::xs)
                              )
                              | _ -> raise BadStack

			)
;;
(* Compile is just a postorder traversal of the tree *)
let rec compile tree = match tree with
    | N(x) -> [NCONST(mk_big x)]
    | B(x) -> [BCONST x]
    | Var(x) -> [NCONST(mk_big 4)]
    | Conjunction(t1,t2) -> (compile t1) @ (compile t2) @ [CONJ]
    | Disjunction(t1,t2) -> (compile t1) @ (compile t2) @ [DISJ]
    | Not(t1) -> (compile t1) @ [NOT]
    | Equals(t1,t2) -> (compile t1) @ (compile t2) @ [EQS]
    | GreaterTE(t1,t2) -> (compile t1) @ (compile t2) @ [GTE]
    | LessTE(t1,t2) -> (compile t1) @ (compile t2) @ [LTE]
    | GreaterT(t1,t2) -> (compile t1) @ (compile t2) @ [GT]
    | LessT(t1,t2) -> (compile t1) @ (compile t2) @ [LT]
    | InParen(t1) -> (compile t1) @ [PAREN]
    | IfThenElse(t1,t2,t3) -> (compile t1) @ (compile t2) @ (compile t3) @ [IFTE]
    | Tuple(x,l1) ->( match l1 with 
                    | [y] -> (compile y) @ [TUPLE(x)]
                    | y::ys -> (compile y) @ (List.rev(List.tl(List.rev (compile (Tuple(x-1,ys)))))) @ [TUPLE(x)]
    ) 
    | Project((i,x),t1) -> (compile t1) @ [PROJ (i,x)]                    
    | Add(t1,t2) ->  (compile t1) @ (compile t2) @ [PLUS]
    | Sub(t1,t2) ->  (compile t1) @ (compile t2) @ [MINUS]
    | Mult(t1,t2) ->  (compile t1) @ (compile t2) @ [MULT]
    | Div(t1,t2) ->  (compile t1) @ (compile t2) @ [DIV]
    | Rem(t1,t2) ->  (compile t1) @ (compile t2) @ [REM]
    | Negative(t1) ->  (compile t1) @ [UNARYMINUS]
    | Abs(t1) ->  (compile t1) @ [ABS]
;; 
