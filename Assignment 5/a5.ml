open A0;;
exception TypeMismatch;;
exception Not_implemented;;
exception InvalidClosure;;
exception BadStack;;
exception Not_found;;
exception IthProjNotPossible;;
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
| GT of closure | GTE of closure| LT of closure|LTE of closure|EQ of closure|ABSOLUTE| NEGA | TUPLE of (closure * closure)
| DIV of closure | REM of closure | PROJECT of int | NOT_stk;;
let int_of_Integer t = match t with
| Integer(x) -> x
| _ -> raise TypeMismatch;; 

let bool_of_Bool b = match b with
| Bool(x) -> x
| _ -> raise TypeMismatch;;

(* This function extracts the bigint from answer type *)
let bigint_of_Num t = match t with 
| Num(x) -> x
| _ -> raise TypeMismatch;;

(* This function extracts bool from answer type *)
let bool_of_BoolVal b = match b with
| BoolVal(x) -> x
| _ -> raise TypeMismatch;;

let rec search x l = match l with
| [] -> raise Not_found
| y::ys -> let (a,b) = y in 
            if x = a then b
            else (search x ys)  
;;
(* Evaluation using krivine machine and call by name semantics 
  foc: focus closure
  stk: stack *)
let rec krivine foc stk = match foc with
  VClosure(e,gamma)->(  
      match e with
      | Num(x) ->( match stk with
                    | [] -> foc 
                    | PLUS_stk(Closure(a,g_dash))::tl -> (krivine (Closure(a,g_dash)) (PLUS_stk(VClosure(Num(x),gamma))::tl))
                    | PLUS_stk(VClosure(Num(x1),g_dash))::tl -> krivine (VClosure(Num(add x x1),gamma)) tl
                    | SUB(Closure(a,g_dash))::tl -> (krivine (Closure(a,g_dash)) (SUB(VClosure(Num(x),gamma))::tl))
                    | SUB(VClosure(Num(x1),g_dash))::tl -> krivine (VClosure(Num(sub x1 x),gamma)) tl
                    | DIV(Closure(a,g_dash))::tl -> (krivine (Closure(a,g_dash)) (DIV(VClosure(Num(x),gamma))::tl))
                    | DIV(VClosure(Num(x1),g_dash))::tl -> krivine (VClosure(Num(div x1 x),gamma)) tl
                    | REM(Closure(a,g_dash))::tl -> (krivine (Closure(a,g_dash)) (REM(VClosure(Num(x),gamma))::tl))
                    | REM(VClosure(Num(x1),g_dash))::tl -> krivine (VClosure(Num(rem x1 x),gamma)) tl
                    | MULT(Closure(a,g_dash))::tl -> (krivine (Closure(a,g_dash)) (MULT(VClosure(Num(x),gamma))::tl))
                    | MULT(VClosure(Num(x1),g_dash))::tl -> krivine (VClosure(Num(mult x x1),gamma)) tl
                    | CMP_stk::tl -> (krivine (VClosure(BoolVal(gt x (mk_big 0)),gamma)) tl)
                    | GT(Closure(a,g_dash))::tl -> (krivine (Closure(a,g_dash)) (GT(VClosure(Num(x),gamma))::tl))
                    | GT(VClosure(Num(x1),g_dash))::tl -> krivine (VClosure(BoolVal(gt x1 x),gamma)) tl
                    | LT(Closure(a,g_dash))::tl -> (krivine (Closure(a,g_dash)) (LT(VClosure(Num(x),gamma))::tl))
                    | LT(VClosure(Num(x1),g_dash))::tl -> krivine (VClosure(BoolVal(lt x1 x),gamma)) tl
                    | GTE(Closure(a,g_dash))::tl -> (krivine (Closure(a,g_dash)) (GTE(VClosure(Num(x),gamma))::tl))
                    | GTE(VClosure(Num(x1),g_dash))::tl -> krivine (VClosure(BoolVal(geq x1 x),gamma)) tl
                    | LTE(Closure(a,g_dash))::tl -> (krivine (Closure(a,g_dash)) (LTE(VClosure(Num(x),gamma))::tl))
                    | LTE(VClosure(Num(x1),g_dash))::tl -> krivine (VClosure(BoolVal(leq x1 x),gamma)) tl
                    | EQ(Closure(a,g_dash))::tl -> (krivine (Closure(a,g_dash)) (EQ(VClosure(Num(x),gamma))::tl))
                    | EQ(VClosure(Num(x1),g_dash))::tl -> krivine (VClosure(BoolVal(eq x1 x),gamma)) tl
                    | ABSOLUTE::tl -> krivine (VClosure(Num(abs x),gamma)) tl
                    | NEGA::tl -> krivine (VClosure(Num(minus x),gamma)) tl
                    | TUPLE(VClosure(Tup(n1,l1),g1),Closure(Tuple(n2,l2),g2))::tl -> if (n2) = 0 then krivine (VClosure(Tup((n1+1,l1@[e])),gamma)) tl
                                                                                     else krivine (Closure((List.hd l2),gamma)) (TUPLE(VClosure(Tup(n1+1,l1@[e]),gamma),Closure(Tuple(n2-1,(List.tl l2)),gamma))::tl)   
                    | _ -> raise BadStack                                    
      )
      | BoolVal(x) -> ( match stk with 
                    [] -> foc
                    | AND(Closure(a,g_dash))::tl -> (krivine (Closure(a,g_dash)) (AND(VClosure(BoolVal(x),gamma))::tl))
                    | AND(VClosure(BoolVal(x1),g_dash))::tl -> krivine (VClosure(BoolVal(x && x1),gamma)) tl
                    | OR(Closure(a,g_dash))::tl -> (krivine (Closure(a,g_dash)) (OR(VClosure(BoolVal(x),gamma))::tl))
                    | OR(VClosure(BoolVal(x1),g_dash))::tl -> krivine (VClosure(BoolVal(x || x1),gamma)) tl 
                    | IFTE(cl1,cl2)::tl -> if x then (krivine cl1 tl) 
                                            else (krivine cl2 tl) 
                    | TUPLE(VClosure(Tup(n1,l1),g1),Closure(Tuple(n2,l2),g2))::tl -> if (n2) = 0 then krivine (VClosure(Tup((n1+1,l1@[e])),gamma)) tl
                                                                                     else krivine (Closure((List.hd l2),gamma)) (TUPLE(VClosure(Tup(n1+1,l1@[e]),gamma),Closure(Tuple(n2-1,(List.tl l2)),gamma))::tl)                                          
                    | NOT_stk::tl -> krivine (VClosure(BoolVal(not x),gamma)) tl
                    | _ -> raise BadStack                                    
      )
      | Tup(n,x) -> (
                    match stk with
                    | [] -> foc 
                    | TUPLE(VClosure(Tup(n1,l1),g1),Closure(Tuple(n2,l2),g2))::tl -> if (n2) = 0 then krivine (VClosure(Tup((n1+1,l1@[e])),gamma)) tl
                                                                                     else krivine (Closure((List.hd l2),gamma)) (TUPLE(VClosure(Tup(n1+1,l1@[e]),gamma),Closure(Tuple(n2-1,(List.tl l2)),gamma))::tl)
                    | PROJECT(i)::tl -> krivine (VClosure((List.nth x (i-1)),gamma)) tl
                    | _ -> raise BadStack
                    )
  )
  | Closure(e,gamma) -> (match e with
          | V(x) -> (print_string x;krivine (search x gamma) stk)
          | Integer(x) -> (krivine (VClosure(Num(mk_big x),gamma)) stk)
          | Bool(x) -> (krivine (VClosure(BoolVal(x),gamma)) stk)
          | Abs(e1) -> krivine (Closure(e1,gamma)) (ABSOLUTE::stk)
          | Negative(e1) -> krivine (Closure(e1,gamma)) (NEGA::stk)
          | Sub(e1,e2) -> (krivine (Closure(e1,gamma)) (SUB(Closure(e2,gamma))::stk) )
          | Cmp(e1) -> (krivine (Closure(e1,gamma)) (CMP_stk::stk))
          | Plus(e1,e2) -> (krivine (Closure(e1,gamma)) (PLUS_stk(Closure(e2,gamma))::stk) )
          | Mult(e1,e2) -> (krivine (Closure(e1,gamma)) (MULT(Closure(e2,gamma))::stk) )
          | Div(e1,e2) -> (krivine (Closure(e1,gamma)) (DIV(Closure(e2,gamma))::stk) )
          | Rem(e1,e2) -> (krivine (Closure(e1,gamma)) (REM(Closure(e2,gamma))::stk) )
          | And(e1,e2) -> (krivine (Closure(e1,gamma)) (AND(Closure(e2,gamma))::stk) )
          | Or(e1,e2) -> (krivine (Closure(e1,gamma)) (OR(Closure(e2,gamma))::stk) )
          | If_Then_Else(e1,e2,e3) -> (krivine (Closure(e1,gamma)) (IFTE(Closure(e2,gamma),(Closure(e3,gamma)))::stk) )
          | Lambda(x,t,e1) -> ( match stk with
                                            | ARG(cl)::tl -> (krivine (Closure(e1,(x,cl)::gamma)) tl)
                                            | _ -> raise InvalidClosure
                                          )
          | RecLambda(x,name,t,e1) -> let p1 = Closure(e1,gamma) in
                                      ( match stk with
                                            | ARG(cl)::tl -> (krivine (Closure(e1,(x,cl)::(name,foc)::gamma)) tl)
                                            | _ -> raise InvalidClosure
                                          )
          | GreaterT(e1,e2) -> krivine (Closure(e1,gamma)) (GT(Closure(e2,gamma))::stk)
          | GreaterTE(e1,e2) -> krivine (Closure(e1,gamma)) (GTE(Closure(e2,gamma))::stk)
          | LessT(e1,e2) -> krivine (Closure(e1,gamma)) (LT(Closure(e2,gamma))::stk)
          | LessTE(e1,e2) -> krivine (Closure(e1,gamma)) (LTE(Closure(e2,gamma))::stk)
          | Equals(e1,e2) -> krivine (Closure(e1,gamma)) (EQ(Closure(e2,gamma))::stk)                                                                   
          | App(e1,e2) -> (krivine (Closure(e1,gamma)) ((ARG(Closure(e2,gamma)))::stk))
          | InParen(e1) -> krivine (Closure(e1,gamma)) stk
          | Tuple(n,l1) -> krivine (Closure((List.hd l1),gamma)) (TUPLE(VClosure(Tup(0,[]),gamma),Closure(Tuple(n-1,List.tl l1),gamma))::stk) 
          | Project((i,n),e) -> krivine (Closure(e,gamma)) (PROJECT(i)::stk)
          | Not(e1) -> krivine (Closure(e1,gamma)) (NOT_stk::stk)
  );;
(* takes first n elements of list l1 in list l2 *)
let rec firstN n l1 l2 = if n == 1 then (l2 @ [List.hd l1],List.tl l1)
                         else 
                         (
                           match l1 with 
                          | x::xs -> firstN (n-1) xs (l2@[x])
                          | _ -> raise BadStack
                         );;      
(* This functions extracts the answer out of Ansclosure *)
let getAns a = match a with
|(Ansclos(x,y)) -> x
| _ -> raise InvalidClosure;;
(* converts list of Ansclosures to answer *)
let rec listToAnswer l1 = match l1 with
|[] -> []
| hd::tl -> (getAns hd)::(listToAnswer tl)
(* SECD machine
stk: stack
env: table 
opc: opcode list
dump: dump *)
let rec secdmc stk env opc dump = match opc with
    | [] -> (match stk with
            | [] -> raise BadStack
            | Ansclos(a,gamma)::xs -> Ansclos(a,env)
    )
    | x::xs -> ( match x with
    | VAR(x) -> let y = (search x env) in (secdmc (y::stk) env xs dump)
    | CLOS(x,c1) -> (secdmc ((Ansclos(Triple(x,c1),env))::stk) env xs dump)
    | RCLOS(x,name,c1) -> (secdmc ((Ansclos(Rectriple(x,name,c1),env))::stk) env xs dump)
    | APP ->( match stk with 
              | a::(Ansclos(Triple(x,c1),gamma))::tl ->(secdmc [] ((x,a)::gamma) c1 (Dump(tl,env,xs)::dump))
              | a::(Ansclos(Rectriple(x,name,c1),gamma))::tl -> let p = (Ansclos(Rectriple(x,name,c1),gamma)) in 
                                                            (secdmc [] ((name,p)::(x,a)::gamma) c1 (Dump(tl,env,xs)::dump)) 
              | _ -> raise BadStack
    )
    | RET -> (match stk with
              | a::tl -> ( match dump with 
                          | Dump(s,gamma,c1)::ds -> (secdmc (a::s) gamma c1 ds)
                          |_ -> raise BadStack
              )
              | _ -> raise BadStack
    )
    | ADD -> (
              match stk with
              | Ansclos(z1,gamma1)::Ansclos(z2,gamma2)::zs -> 
                secdmc (Ansclos(Num(add (bigint_of_Num z2) (bigint_of_Num z1)),env)::zs) env xs dump
              | _ -> raise BadStack
            )
    | SUB -> (
              match stk with
              | Ansclos(z1,gamma1)::Ansclos(z2,gamma2)::zs -> 
                secdmc (Ansclos(Num(sub (bigint_of_Num z2) (bigint_of_Num z1)),env)::zs) env xs dump
              | _ -> raise BadStack
            )        
    | INTO ->(
              match stk with
              | Ansclos(z1,gamma1)::Ansclos(z2,gamma2)::zs -> 
                secdmc (Ansclos(Num(mult (bigint_of_Num z2) (bigint_of_Num z1)),env)::zs) env xs dump
              | _ -> raise BadStack
            )
    | OR -> (
              match stk with
              | Ansclos(z1,gamma1)::Ansclos(z2,gamma2)::zs -> 
                secdmc (Ansclos(BoolVal((bool_of_BoolVal z2) || (bool_of_BoolVal z1)),env)::zs) env xs dump
              | _ -> raise BadStack
            )
    | AND -> (
              match stk with
              | Ansclos(z1,gamma1)::Ansclos(z2,gamma2)::zs -> 
                secdmc (Ansclos(BoolVal((bool_of_BoolVal z2) && (bool_of_BoolVal z1)),env)::zs) env xs dump
              | _ -> raise BadStack
            )
    | BCONST(x) -> secdmc (Ansclos(BoolVal(x),env)::stk) env xs dump
    | NCONST(x) -> secdmc (Ansclos(Num(x),env)::stk) env xs dump
    | CMP ->(match stk with
            |Ansclos(a,gamma)::zs -> secdmc (Ansclos(BoolVal(gt (bigint_of_Num a) (mk_big 0)),env)::zs) env xs dump
            | _ -> raise BadStack   
    )
    | COND(c2,c3) -> (match stk with
            | Ansclos(a,gamma)::zs -> if (bool_of_BoolVal a) then secdmc zs env (c2@xs) dump
                                      else secdmc zs env (c3@xs) dump
            | _ -> raise BadStack   
    )
    | GT ->	(	match stk with
          | Ansclos(z1,gamma1)::Ansclos(z2,gamma2)::zs -> secdmc (Ansclos(BoolVal(gt (bigint_of_Num z2) (bigint_of_Num z1)),env)::zs) env xs dump
          | _ -> raise BadStack
        )
    | EQS -> ( match stk with
          | Ansclos(z1,gamma1)::Ansclos(z2,gamma2)::zs -> secdmc (Ansclos(BoolVal(eq (bigint_of_Num z2) (bigint_of_Num z1)),env)::zs) env xs dump
          | _ -> raise BadStack
    )    
    | GTE ->	(	match stk with
          | Ansclos(z1,gamma1)::Ansclos(z2,gamma2)::zs -> secdmc (Ansclos(BoolVal(geq (bigint_of_Num z2) (bigint_of_Num z1)),env)::zs) env xs dump
          | _ -> raise BadStack
        )
    | LT ->	(	match stk with
          | Ansclos(z1,gamma1)::Ansclos(z2,gamma2)::zs -> secdmc (Ansclos(BoolVal(lt (bigint_of_Num z2) (bigint_of_Num z1)),env)::zs) env xs dump
          | _ -> raise BadStack
        )
    | LTE ->	(	match stk with
          | Ansclos(z1,gamma1)::Ansclos(z2,gamma2)::zs -> secdmc (Ansclos(BoolVal(leq (bigint_of_Num z2) (bigint_of_Num z1)),env)::zs) env xs dump
          | _ -> raise BadStack
        )
    | PAREN -> (	match stk with
          | z1::zs -> secdmc stk env xs dump
          | _ -> raise BadStack
        )
     | DIV ->	 (
              match stk with
              | Ansclos(z1,gamma1)::Ansclos(z2,gamma2)::zs -> 
                secdmc (Ansclos(Num(div (bigint_of_Num z2) (bigint_of_Num z1)),env)::zs) env xs dump
              | _ -> raise BadStack
            )
    | REM ->	 (
              match stk with
              | Ansclos(z1,gamma1)::Ansclos(z2,gamma2)::zs -> 
                secdmc (Ansclos(Num(rem (bigint_of_Num z2) (bigint_of_Num z1)),env)::zs) env xs dump
              | _ -> raise BadStack
            )
    | UNARYMINUS ->	 (
              match stk with
              | Ansclos(z1,gamma1)::zs -> 
                secdmc (Ansclos(Num(minus (bigint_of_Num z1)),gamma1)::zs) env xs dump
              | _ -> raise BadStack
            )
    | ABS -> (match stk with
              | Ansclos(z1,gamma1)::zs -> 
                secdmc (Ansclos(Num(abs (bigint_of_Num z1)),gamma1)::zs) env xs dump
              | _ -> raise BadStack
            )
    | NOT -> (	match stk with
              | Ansclos(z1,gamma1)::zs -> 
                secdmc (Ansclos(BoolVal(not (bool_of_BoolVal z1)),gamma1)::zs) env xs dump
              | _ -> raise BadStack
              )
    | SIMPLEDEF(x) -> (
              match stk with 
              | z1::zs -> 
                secdmc zs ((x,z1)::env) xs dump
              | _ -> raise BadStack  
    )
    (* | RECDEF(x) -> (
              match stk with 
              | Ansclos(Triple(q,c1),g_dash)::zs -> 
                secdmc zs ((x,Ansclos(Rectriple(q,x,c1),g_dash))::env) xs dump
              | _ -> raise BadStack
    ) *)
    | LET ->secdmc stk (List.tl env) xs dump          
    | TUPLE(a) -> if a == 0 then
                    secdmc (Ansclos(Tup(0,[]),env)::stk) env xs dump
                  else 	
                  (
                    if (List.length stk) < a then raise BadStack
                    else(
                      let (l1,l2) = firstN a stk [] in
                      secdmc (Ansclos(Tup(a,(listToAnswer(List.rev l1))),env)::l2) env xs dump
                    )
                  )
    | PROJ(x,y) -> if x > y then raise IthProjNotPossible
                  else 
                  match stk with
                  | Ansclos(Tup(y,l1),gamma)::zs ->( 
                                        secdmc (Ansclos((List.nth (l1) (x-1)),gamma)::zs) env xs dump
                  )
                  | _ -> raise BadStack                            	         	        		
    )
(* compiling the expr tree *)
let rec compile t = match t with 
| V(x) -> [VAR x]
| Negative(t1) ->  (compile t1) @ [UNARYMINUS]
| Abs(t1) ->  (compile t1) @ [ABS]
| Div(t1,t2) ->  (compile t1) @ (compile t2) @ [DIV]
| Rem(t1,t2) ->  (compile t1) @ (compile t2) @ [REM] 
| Lambda(x,t,e1) -> [CLOS(x,(compile e1)@[RET])]  
| App(e1,e2) -> (compile e1) @ (compile e2) @ [APP] 
| Plus(e1,e2) -> (compile e1) @ (compile e2) @ [ADD]
| Sub(e1,e2) -> (compile e1) @ (compile e2) @ [SUB]  
| Mult(e1,e2) -> (compile e1) @ (compile e2) @ [INTO] 
| And(e1,e2) -> (compile e1) @ (compile e2) @ [AND] 
| Or(e1,e2) -> (compile e1) @ (compile e2) @ [OR] 
| Bool(x) -> [BCONST x] 
| Equals(t1,t2) -> (compile t1) @ (compile t2) @ [EQS]
| GreaterTE(t1,t2) -> (compile t1) @ (compile t2) @ [GTE]
| LessTE(t1,t2) -> (compile t1) @ (compile t2) @ [LTE]
| GreaterT(t1,t2) -> (compile t1) @ (compile t2) @ [GT]
| Let(Simple(x,e1),e2) -> (compile e1) @ [SIMPLEDEF(x)] @ (compile e2) @ [LET]
(* | Let(RecLambda(name,e1),e2) -> (compile e1) @ [RECDEF(name)] @ (compile e2) @ [LET]   *)
| RecLambda(x,name,t,e1) -> [RCLOS(x,name,(compile e1)@[RET])]
| Tuple(x,l1) ->( match l1 with
                | [] -> [TUPLE(0)] 
                | [y] -> (compile y) @ [TUPLE(x)]
                | y::ys -> (compile y) @ (List.rev(List.tl(List.rev (compile (Tuple(x-1,ys)))))) @ [TUPLE(x)]
) 
| Project((i,x),t1) -> (compile t1) @ [PROJ (i,x)] 
| LessT(t1,t2) -> (compile t1) @ (compile t2) @ [LT]
| InParen(t1) -> (compile t1) @ [PAREN]
| Integer(x) -> [NCONST (mk_big x)] 
| Cmp(e1) -> (compile e1) @ [CMP] 
| Not(t1) -> (compile t1) @ [NOT]
| If_Then_Else(e1,e2,e3) -> (compile e1) @ [COND(compile e2,compile e3)]
