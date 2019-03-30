open A1
exception Not_implemented
exception Type_mismatch;;
exception Not_found;;
exception Intersection_not_zero;;
(* Searching in a table *)
let rec search x l = match l with
| [] -> raise Not_found
| y::ys -> let (a,b) = y in 
            if x = a then b
            else (search x ys)  
;;
(* if x is a member of list l *)
let ismem x l = let (a,b) = x in
                (try let y = (search a l) in true
                with Not_found -> false)
;;
(* Augments two tables t1 t2*)
let rec augment t1 t2 = match t1 with
|[] -> t2 
|hd::tl -> if (ismem hd t2) = false then (augment tl (t2@[hd]))
            else (augment tl t2) ;;
(* checking if intersection of two tables is phi *)
let rec checkInter t1 t2 = match t1 with 
| [] -> true
| x::xs -> if (ismem x t2) = true then false
           else (checkInter xs t2)
;; 
(* create exptype list from exptree list *)
let rec getTypeList l1 g= match l1 with
  [] -> []
| x::xs -> (getType g x)::(getTypeList xs g)  

and genTable d1 g = match d1 with
| Simple(x,t1) -> [(x,(getType g t1))]
| Sequence (d_list) ->( match d_list with 
                        | [] -> []
                        | d::ds -> let table = (genTable d g) in (augment table (genTable (Sequence(ds)) (augment g table)))
)
| Parallel (d_list) -> ( match d_list with 
                        | [] -> []
                        | d::ds -> let table = (genTable d g) and table2 = (genTable (Parallel(ds)) g)  in 
                                   (if (checkInter table table2) then  (augment table table2)
                                  else raise Intersection_not_zero)
)
| Local(d1,d2) -> (genTable d2 (augment g (genTable d1 g)))
and getType g e = match e with
  | Var(x) -> (try (search x g)
              with Not_found -> raise Type_mismatch)  
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
  | Conjunction(t1,t2) -> if (getType g t1 = Tbool) && (getType g t2 = Tbool) then Tbool
                  else raise Type_mismatch     (* conjunction /\ *)
  | Disjunction(t1,t2) -> if (getType g t1 = Tbool) && (getType g t2 = Tbool) then Tbool
                  else raise Type_mismatch   (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals(t1,t2) -> if (getType g t1 = Tint) && (getType g t2 = Tint) then Tbool
                     else raise Type_mismatch        (* = *)
  | GreaterTE(t1,t2) -> if (getType g t1 = Tint) && (getType g t2 = Tint) then Tbool
                     else raise Type_mismatch     (* >= *)
  | LessTE(t1,t2) -> if (getType g t1 = Tint) && (getType g t2 = Tint) then Tbool
                     else raise Type_mismatch        (* <= *)
  | GreaterT(t1,t2) -> if (getType g t1 = Tint) && (getType g t2 = Tint) then Tbool
                     else raise Type_mismatch      (* > *)
  | LessT(t1,t2) -> if (getType g t1 = Tint) && (getType g t2 = Tint) then Tbool
                     else raise Type_mismatch         (* < *)
  (* expressions using parenthesis *)
  | InParen(t1) -> getType g t1               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse(t1,t2,t3) -> if (getType g t1) = Tbool && (getType g t2 = getType g t3) then (getType g t2)
                            else raise Type_mismatch
  (* creating n-tuples (n >= 0) *)
  | Tuple(x,l1) -> if x != (List.length l1) then raise Type_mismatch
                  else Ttuple(getTypeList l1 g)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project((x,y),t1) -> (match (getType g t1) with 
                        | Ttuple(l1) -> (List.nth l1 x)
                        | _ -> raise Type_mismatch)
  | Let (d,t1) -> let generated_table = (genTable d g) in
                  (getType (augment g generated_table) t1)
  | FunctionAbstraction(x,e1) -> (try let ty = (search x g) in
                                  Tfunc(ty,(getType (augment g [(x,ty)]) e1)) 
                                  with Not_found -> raise Type_mismatch
  )                
  | FunctionCall (e1,e2) ->( match (getType g e1) with
                            | Tfunc(t1,t2) -> if (getType g e2) = t1 then t2
                                              else raise Type_mismatch   
                            | _ -> raise Type_mismatch  
  )
and hastype g e t = match e with
| FunctionAbstraction (x,e1) ->(
                                match t with
                                |Tfunc(t1,t2) -> (hastype (augment g ([x,t1])) e1 t2)
                                |_ -> false
) 
| _ -> ( try (getType g e) = t
         with Type_mismatch -> false 
)
;;
(* yields : ((string * exptree) list) -> definition -> ((string * exptree) list) -> bool *)
let rec yields g d g_dash = try ((List.rev(genTable d g)) = g_dash) 
                            with Type_mismatch -> false  ;;  
