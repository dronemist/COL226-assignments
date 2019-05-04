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
type actions = Command of string * exptree | FunctionCall of (string * (exptree list)) | Return | PrintStk 
| PrintSL | PrintVar | PrintProc | PrintOptions;;

(* Types of the expressions, Tnull denotes the default type for the expression *)
type exptype = Tint | Tbool;;

(* answer type, the type of values that will be assigned to the variables *)
type answer = NumVal of int | BoolVal of bool;; 

(* Type of the main data structure *)
type frameElements = Name of string | LocalVals of ((string * exptype * answer) list) 
| Arg of ((string * exptype * answer) list) | Ret of int | StaticLink of int | Blank ;;

(* current stack pointer
let currPointer = ref 0;;
(* Initial stack *)
let stack = ref ([(Name("main"));LocalVals([("a",Tint,NumVal(0));("b",Tint,NumVal(0));("c",Tint,NumVal(0))])]);; *)


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
| "P" -> if b = "Q" || b = "R" || b = "S"|| b = "P" then true else false
| "Q" -> if b = "P" || b = "T" || b = "U"|| b = "Q" then true else false
| "R" -> if b = "S" || b = "V" || b = "P"|| b = "R" || b = "Q" then true else false
| "S" -> if b = "R" || b = "P" || b = "Q"|| b = "S" then true else false
| "T" -> if b = "W" || b = "U" || b = "P"|| b = "T" || b = "Q" then true else false
| "U" -> if b = "T" || b = "P" || b = "Q"|| b = "U" then true else false
| "V" -> if b = "R" || b = "S" || b = "P"|| b = "V" || b = "Q" then true else false
| "W" -> if b = "T" || b = "U" || b = "P"|| b = "W" || b = "Q" then true else false
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
    ( 
      let Name(proc) = (List.nth !stack currPointer) in
      (
        Printf.printf "Update occurs in procedure %s" proc; 
        stack := (firstN (currPointer + 1) !stack) @ [LocalVals(l2)] @ (lastN ((List.length !stack) - currPointer - 2) !stack);
        flush stdout;
      )
    )
    )
  with Not_found -> (
    if currPointer > 0 then 
    ( try(  
        match (List.nth !stack (currPointer - 3)) with
      | Arg(l1) -> let l2 = (findAndReplace x l1 e_type a) in 
      ( 
        let Name(proc) = (List.nth !stack currPointer) in
        (
          Printf.printf "Update occurs in procedure %s" proc;
          stack := (firstN (currPointer - 3) !stack) @ [Arg(l2)] @ (lastN ((List.length !stack) - currPointer + 2) !stack);
          flush stdout;
        )
      )
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
stack := (firstN (x+2) !stack); currPointer := x
)
else (stack := []; currPointer := (-1));;

(* function to execute assignment *)
let assign x e stack currPointer = let a = (eval e !stack !currPointer) and e_type = (getType e) in 
(assignHelper x a e_type stack !currPointer);;

(* Function to display static link *)
let rec displaySL stack currPointer = if currPointer>=0 then
(
  let Name(s) = (List.nth stack currPointer) in (print_string (s ^ " "));
  if currPointer>=1 then
  (let StaticLink(i) = (List.nth stack (currPointer-1)) in (displaySL stack i));
)

(* function to display the call stack *)
let rec displayStk stack currPointer = if currPointer >=0 then 
(
  let Name(s) = (List.nth stack currPointer) in (print_string (s ^ " "));
    (displayStk stack (currPointer-5));
)
;;

(* function to print procedures that can be called *)
let canCallPrint stack currPointer = let Name(a) = (List.nth stack currPointer) in
(
  match a with 
| "main" -> print_string "P and Q " ;
| "P" -> print_string "Q, R, S and P"
| "Q" -> print_string "P, T, U and Q ";
| "R" -> print_string "S, V, P, R and Q";
| "S" -> print_string "R, P, Q and S " ;
| "T" -> print_string "W, U, P, T and Q";
| "U" -> print_string "T, P, Q and U ";
| "V" -> print_string "R, S, P, V and Q";
| "W" -> print_string "T, U, P, W and Q";
);;

(* convert answer to string *)
let expToString e = match e with
|NumVal(x) -> (string_of_int x)
|BoolVal(b) -> (string_of_bool b)
;;

(* To check if x is present in a given variable list *)
let rec isMem x l1 = match l1 with
|[] -> false
|(y,t1,v1)::ys -> if (y=x) then true else (isMem x ys)
;;

(* takes union of l1 and l2 *)
let rec listUnion l1 l2 = match l2 with
| [] -> l1
| (y2,t2,v2)::ys -> if(isMem y2 l1) then (listUnion l1 ys)
                    else (listUnion (l1 @ ([(y2,t2,v2)]))  l2)

;;
(* Function to print variable list *)
let rec printVarList l1 = match l1 with 
| [] -> print_string "";
| (x,t,v)::xs -> print_string (x ^ " = " ^ (expToString v) ^ "\n");
                 printVarList xs;
;;

let rec retVarList stack currPointer = 
  let LocalVals(l1) = (List.nth stack (currPointer + 1)) in
  (
    if(currPointer>0) then (
      let Arg(l2) = (List.nth stack (currPointer - 3)) in 
      (
        let StaticLink(n) = (List.nth stack (currPointer - 1)) in 
        listUnion (listUnion l1 l2) (retVarList stack n)
      ) 
    )
    else l1  
  )  
;;

(* Function to respond to actions *)
let main a stack currPointer = match a with
| FunctionCall(s,l1) -> (callProcedure s l1 stack currPointer)
| Return -> (returnFromProcedure stack currPointer)
| Command(s,e) -> (assign s e stack currPointer)
| PrintStk -> displayStk !stack !currPointer;
| PrintSL -> displaySL !stack !currPointer;
| PrintProc -> canCallPrint !stack !currPointer;
| PrintVar -> printVarList(retVarList !stack !currPointer);
| PrintOptions -> print_string "#1 = Stack\n#2 = Procedures that can be called\n#3 = Variables that can accessed\n#4 = Static link chain";
;;