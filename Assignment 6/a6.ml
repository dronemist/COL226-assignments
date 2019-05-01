exception Wrong_arguments;;
exception Cant_call;;
exception Variable_cant_be_called;;
exception Not_found;;
exception Type_mismatch_in_assignment;;
exception Variable_cant_be_accessed;;

type  exptree =
  V of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | Integer of int      (* Integer constant *)
  | Bool of bool     (* Boolean constant *);;

(* actions *)
type actions = Command of string * exptree | FunctionCall of (string * (exptree list)) | Return ;;

(* Types of the expressions, Tnull denotes the default type for the expression *)
type exptype = Tint | Tbool;;

(* answer type, the type of values that will be assigned to the variables *)
type answer = NumVal of int | BoolVal of bool;; 

(* Type of the main data structure *)
type frameElements = Name of string | LocalVals of ((string * exptype * answer) list) 
| Arg of ((string * exptype * answer) list) | Ret of int | StaticLink of int | Blank ;;

(* The tree structure of procedure *)
type procTree = Node of (procTree * string * procTree) | Leaf ;;

(* getType of get a type of an exptree x *)
let getType x = match x with
| Integer(i) -> Tint
| Bool(b) -> Tbool
| V(s) -> (match s with
| _ -> Tint)

(* Get parent of x in procTree t1, here x is a string *)
let rec getParent x = match x with
| "P" -> "main"
| "Q" -> "main"
| "R" -> "P"
| "S" -> "P"
| "T" -> "Q"
| "U" -> "Q"
| "V" -> "R"
| "W" -> "T"
;;

(* to check if procedure a can call b *)
let canCall a b = match a with 
| "main" -> if b = "P" || b = "Q" then true else false
| "P" -> if b = "Q" || b = "R" || b = "S" then true else false
| "Q" -> if b = "P" || b = "T" || b = "U" then true else false
| "R" -> if b = "S" || b = "V" || b = "P" || b = "Q" then true else false
| "S" -> if b = "R" || b = "P" || b = "Q" then true else false
| "T" -> if b = "W" || b = "U" || b = "P" || b = "Q" then true else false
| "U" -> if b = "T" || b = "P" || b = "Q" then true else false
| "V" -> if b = "R" || b = "S" || b = "P" || b = "Q" then true else false
| "W" -> if b = "T" || b = "U" || b = "P" || b = "Q" then true else false
;;

(* search for latest occurence of procedure in stack *)
let rec search x stack currPointer = if currPointer>=0 then 
(
match (List.nth stack currPointer) with
| Name(p) -> if p=x then currPointer else (search x stack (currPointer-5))
)
else (-1);;

(* get pointer of the parent *)
let getParentId p stack currPointer = (search (getParent p) stack currPointer);;

(* current stack pointer *)
let currPointer = ref 0;;
let stack = ref ([(Name("main"));LocalVals([("a",Tint,NumVal(2));("b",Tint,NumVal(3));("c",Tint,NumVal(4))])]);;

(* searching in list for variable x *)
let rec searchList x l1 = match l1 with
| [] -> raise Not_found
|(s,et,i)::xs -> if s=x then i else (searchList x xs)
;;

(* extract the value of the variable x *)
let rec extractVbl x stack currPointer = if currPointer >= 0 then
( try (
    match (List.nth stack (currPointer + 1)) with
    | LocalVals(l1) -> (searchList x l1)
  )
  with Not_found -> (
    if currPointer > 0 then 
    ( try(
        match (List.nth stack (currPointer - 3)) with
      | Arg(l1) -> (searchList x l1)
      )
      with Not_found -> let StaticLink(n) = (List.nth stack (currPointer - 1)) in (extractVbl x stack n)
    )
    else
    raise Variable_cant_be_called 
  )
)
else 
raise Variable_cant_be_called;; 

(* returning first n elements of the list *)
let rec firstN n l1 = if n = 1 then ([List.hd l1])
                         else 
                         (
                           match l1 with 
                          | x::xs -> x::(firstN (n-1) xs) 
                         );;

(* returning last n elements of the list *)
let rec lastN n l1 = if n = 0 then []
                         else 
                         (
                           let l2 = (List.rev l1) in 
                          (lastN (n-1) (List.rev(List.tl l2))) @ [List.hd l2]
                         );;

(* evaluating the tree to be assigned *)
let rec eval t1 stack currPointer = match t1 with 
| Integer(i) -> (NumVal i)
| Bool(b) -> (BoolVal b)
| V(x) -> (extractVbl x stack currPointer)

(* function for adding frames *)
let createFrame proc l1 stack currPointer = 
let x1 = (eval (List.nth l1 0) (!stack) !currPointer) and x2 = (eval (List.nth l1 1) (!stack) !currPointer) in
(
match proc with

| "P" -> if (List.length l1) = 2 && (getType (List.nth l1 0) = Tint) && (getType (List.nth l1 1) = Tint) then
let parent_id = (getParentId proc (!stack) (!currPointer)) in
(stack :=  (!stack) @ [(Arg([("x",Tint,x1);("y",Tint,x2)]));Ret(!currPointer);StaticLink(parent_id);Name(proc);LocalVals([("z",Tint,NumVal(0));("a",Tint,NumVal(0))])];
currPointer := (!currPointer) + 5)
else raise Wrong_arguments

| "Q" -> if (List.length l1) = 2 && (getType (List.nth l1 0) = Tint) && (getType (List.nth l1 1) = Tint) then
let parent_id = (getParentId proc (!stack) (!currPointer)) in
(stack :=  (!stack) @ [(Arg([("z",Tint,x1);("w",Tint,x2)]));Ret(!currPointer);StaticLink(parent_id);Name(proc);LocalVals([("x",Tint,NumVal(0));("b",Tint,NumVal(0))])];
currPointer := (!currPointer) + 5)
else raise Wrong_arguments

| "R" -> if (List.length l1) = 2 && (getType (List.nth l1 0) = Tint) && (getType (List.nth l1 1) = Tint) then
let parent_id = (getParentId proc (!stack) (!currPointer)) in
(stack :=  (!stack) @ [(Arg([("w",Tint,x1);("i",Tint,x2)]));Ret(!currPointer);StaticLink(parent_id);Name(proc);LocalVals([("j",Tint,NumVal(0));("b",Tint,NumVal(0))])];
currPointer := (!currPointer) + 5)
else raise Wrong_arguments

| "S" -> if (List.length l1) = 2 && (getType (List.nth l1 0) = Tint) && (getType (List.nth l1 1) = Tint) then
let parent_id = (getParentId proc (!stack) (!currPointer)) in
(stack :=  (!stack) @ [(Arg([("c",Tint,x1);("k",Tint,x2)]));Ret(!currPointer);StaticLink(parent_id);Name(proc);LocalVals([("m",Tint,NumVal(0));("n",Tint,NumVal(0))])];
currPointer := (!currPointer) + 5)
else raise Wrong_arguments

| "T" -> if (List.length l1) = 2 && (getType (List.nth l1 0) = Tint) && (getType (List.nth l1 1) = Tint) then
let parent_id = (getParentId proc (!stack) (!currPointer)) in
(stack :=  (!stack) @ [(Arg([("a",Tint,x1);("y",Tint,x2)]));Ret(!currPointer);StaticLink(parent_id);Name(proc);LocalVals([("i",Tint,NumVal(0));("f",Tint,NumVal(0))])];
currPointer := (!currPointer) + 5)
else raise Wrong_arguments

| "U" -> if (List.length l1) = 2 && (getType (List.nth l1 0) = Tint) && (getType (List.nth l1 1) = Tint) then
let parent_id = (getParentId proc (!stack) (!currPointer)) in
(stack :=  (!stack) @ [(Arg([("c",Tint,x1);("z",Tint,x2)]));Ret(!currPointer);StaticLink(parent_id);Name(proc);LocalVals([("j",Tint,NumVal(0));("b",Tint,NumVal(0))])];
currPointer := (!currPointer) + 5)
else raise Wrong_arguments

| "V" -> if (List.length l1) = 2 && (getType (List.nth l1 0) = Tint) && (getType (List.nth l1 1) = Tint) then
let parent_id = (getParentId proc (!stack) (!currPointer)) in
(stack :=  (!stack) @ [(Arg([("m",Tint,x1);("n",Tint,x2)]));Ret(!currPointer);StaticLink(parent_id);Name(proc);LocalVals([("c",Tint,NumVal(0))])];
currPointer := (!currPointer) + 5)
else raise Wrong_arguments

| "W" -> if (List.length l1) = 2 && (getType (List.nth l1 0) = Tint) && (getType (List.nth l1 1) = Tint) then
let parent_id = (getParentId proc (!stack) (!currPointer)) in
(stack :=  (!stack) @ [(Arg([("m",Tint,x1);("p",Tint,x2)]));Ret(!currPointer);StaticLink(parent_id);Name(proc);LocalVals([("j",Tint,NumVal(0));("h",Tint,NumVal(0))])];
currPointer := (!currPointer) + 5)
else raise Wrong_arguments);;

(* find variable x in l1 and assign it answer a, a has type e_type *)
let rec findAndReplace x l1 e_type a = match l1 with
| [] -> raise Not_found
| (s,et,i)::ys -> if s=x then 
(
  if e_type = et then (s,et,a)::ys
  else raise Type_mismatch_in_assignment
) 
else (s,et,i)::(findAndReplace x ys e_type a)
;;
(* helper function to respond to assign action, assign x the value of a (exptree) *)
let rec assignHelper x a e_type stack currPointer = if currPointer >= 0 then
( try (
    match (List.nth !stack (currPointer + 1)) with
    | LocalVals(l1) -> let l2 = (findAndReplace x l1 e_type a) in
    stack := (firstN (currPointer + 1) !stack) @ [LocalVals(l2)] @ (lastN ((List.length !stack) - currPointer - 2) !stack)
  )
  with Not_found -> (
    if currPointer > 0 then 
    ( try(
        match (List.nth !stack (currPointer - 3)) with
      | Arg(l1) -> let l2 = (findAndReplace x l1 e_type a) in
      stack := (firstN (currPointer - 3) !stack) @ [Arg(l2)] @ (lastN ((List.length !stack) - currPointer + 2) !stack)
      )
      with Not_found -> let StaticLink(n) = (List.nth !stack (currPointer - 1)) in (assignHelper x a e_type stack n)
    )
    else
    raise Variable_cant_be_accessed
  )
)
else 
raise Variable_cant_be_accessed;; 

(* the main function which acts on function call *)
let callProcedure p l1 stack currPointer = match (List.nth (!stack) (!currPointer))
with Name(p2) -> if (canCall p2 p) then (createFrame p l1 stack currPointer) 
else raise Cant_call;;

(* function to execute return action *)
let returnFromProcedure stack currPointer = if (!currPointer) > 0 then
(
let Ret(x) = (List.nth !stack ((!currPointer) - 2)) in 
stack := (firstN (x+2) !stack); currPointer := x)
else (stack := []; currPointer := (-1));;

(* function to execute assignment *)
let assign x e stack currPointer = let a = (eval e !stack !currPointer) and e_type = (getType e) in 
(assignHelper x a e_type stack !currPointer);;