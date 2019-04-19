(* Dummy implementation of A1 *)
open A0
exception NtupleNotPossible;;
exception TypeMismatch;;
exception IthProjNotPossible;;
exception InvalidProjection;;
exception Not_implemented;;
exception BadStack;;
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
(* Answer returned by SECD *)
type answer = Num of bigint | BoolVal of bool | Triple of (string * opcode list);;

type answerClosure = Ansclos of (answer * (string -> answerClosure));;

type dumpType = Dump of (answerClosure list * (string -> answerClosure) * opcode list);;

let int_of_NumVal t = match t with
| NumVal(x) -> x
| _ -> raise TypeMismatch;; 

let bool_of_BoolVal b = match b with
| BoolVal(x) -> x
| _ -> raise TypeMismatch;;

(* This function extracts the bigint from answer type *)
let bigint_of_Num t = match t with 
| Num(x) -> x
| _ -> raise TypeMismatch;;

(* This function extracts bool from answer type *)
let bool_of_Bool b = match b with
| Bool(x) -> x
| _ -> raise TypeMismatch;;

(* evaluating the tree where is the hashtable containing values *)
let rec eval t rho = match t with
    | N(x) -> (NumVal x)
    | B(x) -> (BoolVal x)
    | Var(x) -> (rho x)
    | Conjunction(t1,t2) -> BoolVal(bool_of_BoolVal(eval t1 rho) && bool_of_BoolVal(eval t2 rho))
    | Disjunction(t1,t2) -> BoolVal(bool_of_BoolVal(eval t1 rho) || bool_of_BoolVal(eval t2 rho))
    | Not(t1) ->  BoolVal(not (bool_of_BoolVal(eval t1 rho)))
    | Equals(t1,t2) -> BoolVal( (int_of_NumVal(eval t1 rho)) = (int_of_NumVal(eval t2 rho)))
    | GreaterTE(t1,t2) -> BoolVal((int_of_NumVal(eval t1 rho)) >= (int_of_NumVal(eval t2 rho)))
    | LessTE(t1,t2) -> BoolVal((int_of_NumVal(eval t1 rho)) <= (int_of_NumVal(eval t2 rho)))
    | GreaterT(t1,t2) -> BoolVal((int_of_NumVal(eval t1 rho)) > (int_of_NumVal(eval t2 rho)))
    | LessT(t1,t2) -> BoolVal((int_of_NumVal(eval t1 rho)) < (int_of_NumVal(eval t2 rho)))
    | InParen(t1) -> (eval t1 rho)
    | IfThenElse(t1,t2,t3) -> if bool_of_BoolVal(eval t1 rho) then (eval t2 rho)
                              else (eval t3 rho)
    | Tuple(x,l1) -> if x != (List.length l1) then raise NtupleNotPossible 
                     else 
                      (
                        match l1 with 
                        | [] -> TupVal(0,[])
                        | [y] -> TupVal(x,[eval y rho])
                        | y::ys -> let TupVal(a,list) = (eval (Tuple(x-1,ys)) rho) in
                                      TupVal(x,(eval y rho)::list)
                      )
    | Project((i,x),t1) -> if i > x then raise IthProjNotPossible
                        else 
                        (match (eval t1 rho) with
                        | TupVal(a,list) -> if a != x then raise InvalidProjection
                                           else 
                                           (List.nth list (i-1)) 
                        | _ -> raise IthProjNotPossible
                        )                      
    | Add(t1,t2) -> NumVal((int_of_NumVal(eval t1 rho)) + (int_of_NumVal(eval t2 rho)))
    | Sub(t1,t2) -> NumVal((int_of_NumVal(eval t1 rho)) - (int_of_NumVal(eval t2 rho)))
    | Mult(t1,t2) -> NumVal( (int_of_NumVal(eval t1 rho)) * (int_of_NumVal(eval t2 rho)))
    | Div(t1,t2) -> NumVal( (int_of_NumVal(eval t1 rho)) / (int_of_NumVal(eval t2 rho)))
    | Rem(t1,t2) -> NumVal( (int_of_NumVal(eval t1 rho)) mod (int_of_NumVal(eval t2 rho)))
    | Negative(t1) -> NumVal( -(int_of_NumVal(eval t1 rho)))
    | Abs(t1) -> NumVal( let q = (int_of_NumVal(eval t1 rho)) in
                          if(q >= 0) then q else -q
                        )
;;
(* function to take first n elements of a list *)
let rec firstN n l1 l2 = if n == 1 then (l2 @ [List.hd l1],List.tl l1)
                         else 
                         (
                           match l1 with 
                          | x::xs -> firstN (n-1) xs (l2@[x])
                          | _ -> raise BadStack
                         );;    
(* stackmc evaluated the opcode list *)
let rec stackmc stk rho pgm = match pgm with
    | [] -> (match stk with
            | [] -> raise BadStack
            | x::xs -> x
    )
    | x::xs -> ( match x with
                | NCONST (y) -> stackmc ((Num(y))::stk) rho xs
                | BCONST (y) -> stackmc ((Bool(y))::stk) rho xs
                | VAR (y) -> stackmc ((rho y)::stk) rho xs
                | PLUS -> (	(* The list should have atleast two elements if we match a binary operator *)
                      match stk with
                      | z1::z2::zs -> stackmc (Num(add (bigint_of_Num z2) (bigint_of_Num z1))::zs) rho xs
                      | _ -> raise BadStack
                    )	
                | MINUS ->  (	match stk with
                      | z1::z2::zs -> stackmc (Num(sub (bigint_of_Num z2) (bigint_of_Num z1))::zs) rho xs
                      | _ -> raise BadStack
                    )
                | MULT ->  (	match stk with
                      | z1::z2::zs -> stackmc (Num(mult (bigint_of_Num z2) (bigint_of_Num z1))::zs) rho xs
                      | _ -> raise BadStack
                    )
                | DIV ->	(	match stk with
                      | z1::z2::zs -> stackmc (Num(div (bigint_of_Num z2) (bigint_of_Num z1))::zs) rho xs
                      | _ -> raise BadStack
                    )
                | REM ->	(	match stk with
                      | z1::z2::zs -> stackmc (Num(rem (bigint_of_Num z2) (bigint_of_Num z1))::zs) rho xs
                      | _ -> raise BadStack
                    ) 
                | UNARYMINUS ->	(	
                      match stk with
                      | z1::zs -> stackmc ((Num(minus (bigint_of_Num z1)))::zs) rho xs
                      | _ -> raise BadStack
                    )
                | ABS -> (	match stk with
                      | z1::zs -> stackmc ((Num(abs (bigint_of_Num z1)))::zs) rho xs
                      | _ -> raise BadStack
                    )
                | NOT -> (	match stk with
                      | z1::zs -> stackmc ((Bool(not (bool_of_Bool z1)))::zs) rho xs
                      | _ -> raise BadStack
                    )    
                | EQS ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool(eq (bigint_of_Num z2) (bigint_of_Num z1)))::zs) rho xs
                      | _ -> raise BadStack
                    )
                | CONJ ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool((bool_of_Bool z1) && (bool_of_Bool z2)))::zs) rho xs
                      | _ -> raise BadStack
                    )
                | DISJ ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool((bool_of_Bool z1) || (bool_of_Bool z2)))::zs) rho xs
                      | _ -> raise BadStack
                    )
                | GT ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool(gt (bigint_of_Num z2) (bigint_of_Num z1)))::zs) rho xs
                      | _ -> raise BadStack
                    )
                | GTE ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool(geq (bigint_of_Num z2) (bigint_of_Num z1)))::zs) rho xs
                      | _ -> raise BadStack
                    )
                | LT ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool(lt (bigint_of_Num z2) (bigint_of_Num z1)))::zs) rho xs
                      | _ -> raise BadStack
                    )
                | LTE ->	(	match stk with
                      | z1::z2::zs -> stackmc ((Bool(leq (bigint_of_Num z2) (bigint_of_Num z1)))::zs) rho xs
                      | _ -> raise BadStack
                    )
                | PAREN -> (	match stk with
                      | z1::zs -> stackmc ((z1)::zs) rho xs
                      | _ -> raise BadStack
                    )
                | IFTE ->	(	match stk with
                      | z1::z2::z3::zs -> if (bool_of_Bool z3) then (stackmc (z2::zs) rho xs) 
                        else stackmc (z1::zs) rho xs
                      | _ -> raise BadStack
                    )
                | TUPLE(a) -> if a == 0 then
                                stackmc (Tup(0,[])::stk) rho xs
                              else 	
                              (
                                if (List.length stk) < a then raise BadStack
                                else(
                                  let (l1,l2) = firstN a stk [] in
                                  stackmc (Tup(a,(List.rev l1))::l2) rho xs
                                )
                              )
                | PROJ(x,y) ->if x > y then raise IthProjNotPossible
                              else 
                              match stk with
                              | Tup(y,l1)::zs ->( 
                                                    stackmc ((List.nth (l1) (x-1))::zs) rho xs
                              )
                              | _ -> raise BadStack

			)
;;
(* Compile is just a postorder traversal of the tree *)
let rec compile tree = match tree with
    | N(x) -> [NCONST(mk_big x)]
    | B(x) -> [BCONST x]
    | Var(x) -> [VAR x]
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
                    | [] -> [TUPLE(0)] 
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
