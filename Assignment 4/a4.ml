open A1
exception Not_implemented
exception Type_mismatch;;
(* returns the type of expression e *)
(* let typeOF e =  *)
(* Generates a table for a definition d1 *)
let rec genTable d1 = match d1 with
| Simple(x,t1) -> [(x,(typeOF t1))]
| Sequence (d_list) -> (List.map genTable d_list)
| Parallel (d_list) -> (List.map genTable d_list)
| Local(d1,d2) -> (genTable d2);;

(* augmenting two tables *)
let augment t1 t2 = (t2)@(t1);;

(* Compares types of one exptree list l1 with exptype list l2 *)
let rec hasTypeList l1 l2 g = match l1 with
| [] -> (match l2 with
        | [] -> true
        | _ -> false)
| x::xs ->(match l2 with
        | [] -> false
        | y::ys -> (hastype g x y) && (hasTypeList xs ys g)
);;  

(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec getType g e = match e with
  | Var(x) -> (g x)   
  | N(x) -> Tint      (* Integer constant *)
  | B(x) -> Tbool       (* Boolean constant *)
  (* unary operators on integers *)
  | Abs(t1) -> if (getType g t1) = Tint then Tint
               else raise Type_mismatch (* abs *)
  | Negative(t1) -> if (getType g t1) = Tint then Tint
               else raise Type_mismatch (* abs *)
  (* unary operators on booleans *)
  | Not(t1) -> if (getType g t1) = Tbool then Tbool
               else raise Type_mismatch (* abs *)
  (* binary operators on integers *)
  | Add(t1,t2) -> if (getType g t1 = Tint) && (getType g t2 = Tint) then Tint
                  else raise Type_mismatch         (* Addition + *)
  | Sub(t1,t2) -> if (getType g t1 = Tint) && (getType g t2 = Tint) then Tint
                  else raise Type_mismatch             (* Subtraction - *)
  | Mult(t1,t2) -> if (getType g t1 = Tint) && (getType g t2 = Tint) then Tint
                  else raise Type_mismatch            (* Multiplication * *)
  | Div(t1,t2) -> if (getType g t1 = Tint) && (getType g t2 = Tint) then Tint
                  else raise Type_mismatch             (* div *)
  | Rem(t1,t2) -> if (getType g t1 = Tint) && (getType g t2 = Tint) then Tint
                  else raise Type_mismatch             (* mod *)
  (* binary operators on booleans *)
  | Conjunction(t1,t2) -> ((hastype g t1 Tbool) && (hastype g t2 Tbool) && t = Tbool)   (* conjunction /\ *)
  | Disjunction(t1,t2) -> ((hastype g t1 Tbool) && (hastype g t2 Tbool) && t = Tbool)   (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tbool)        (* = *)
  | GreaterTE(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tbool)     (* >= *)
  | LessTE(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tbool)        (* <= *)
  | GreaterT(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tbool)      (* > *)
  | LessT(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tbool)         (* < *)
  (* expressions using parenthesis *)
  | InParen(t1) -> (hastype g t1 t)               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse(t1,t2,t3) -> ((hastype g t1 Tbool) && (hastype g t2 t) && (hastype g t3 t))  
  (* creating n-tuples (n >= 0) *)
  | Tuple(x,l1) -> if x != (List.length l1) then false
                  else
                  (match t with
                  | Ttuple(l2) -> (hasTypeList l1 l2 g) 
                  | _ -> false
  )
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project((x,y),t1) -> (match t1 with 
                        | Tuple(y,l1) -> (hastype g (List.nth l1 x) t)
                        | _ -> false)
  | Let (d,t1) -> let generated_table = (genTable d) in
                  (hastype t1 (augment g generated_table) t)

  | FunctionAbstraction (x,e) ->(match t with
                                |Tfunc(t1,t2) -> (hastype (augment g ([x,t1])) e t2)
                                |_ -> false
  
  )
  | FunctionCall (e1,e2) -> (hastype g e1 (Tfunc(t1,t))) && (hastype g e2 t1)  
                            
  );;


(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool
let rec hastype g e t = match e with
  | Var(x) -> (g x) = t   
  | N(x) -> t = Tint       (* Integer constant *)
  | B(x) -> t = Tbool       (* Boolean constant *)
  (* unary operators on integers *)
  | Abs(t1) -> (hastype g t1 Tint) && t = Tint                 (* abs *)
  | Negative(t1) -> (hastype g t1 Tint) && t = Tint
  (* unary operators on booleans *)
  | Not(t1) -> (hastype g t1 Tbool) && t = Tbool
  (* binary operators on integers *)
  | Add(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tint)          (* Addition + *)
  | Sub(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tint)           (* Subtraction - *)
  | Mult(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tint)          (* Multiplication * *)
  | Div(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tint)           (* div *)
  | Rem(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tint)           (* mod *)
  (* binary operators on booleans *)
  | Conjunction(t1,t2) -> ((hastype g t1 Tbool) && (hastype g t2 Tbool) && t = Tbool)   (* conjunction /\ *)
  | Disjunction(t1,t2) -> ((hastype g t1 Tbool) && (hastype g t2 Tbool) && t = Tbool)   (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tbool)        (* = *)
  | GreaterTE(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tbool)     (* >= *)
  | LessTE(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tbool)        (* <= *)
  | GreaterT(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tbool)      (* > *)
  | LessT(t1,t2) -> ((hastype g t1 Tint) && (hastype g t2 Tint) && t = Tbool)         (* < *)
  (* expressions using parenthesis *)
  | InParen(t1) -> (hastype g t1 t)               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse(t1,t2,t3) -> ((hastype g t1 Tbool) && (hastype g t2 t) && (hastype g t3 t))  
  (* creating n-tuples (n >= 0) *)
  | Tuple(x,l1) -> if x != (List.length l1) then false
                  else
                  (match t with
                  | Ttuple(l2) -> (hasTypeList l1 l2 g) 
                  | _ -> false
  )
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project((x,y),t1) -> (match t1 with 
                        | Tuple(y,l1) -> (hastype g (List.nth l1 x) t)
                        | _ -> false)
  | Let (d,t1) -> let generated_table = (genTable d) in
                  (hastype t1 (augment g generated_table) t)

  | FunctionAbstraction (x,e) ->(match t with
                                |Tfunc(t1,t2) -> (hastype (augment g ([x,t1])) e t2)
                                |_ -> false
  
  )
  | FunctionCall (e1,e2) -> (hastype g e1 (Tfunc(t1,t))) && (hastype g e2 t1)  
                            
  );; *)


(* yields : ((string * exptree) list) -> definition -> ((string * exptree) list) -> bool *)
let rec yields g d g_dash = raise Not_implemented
