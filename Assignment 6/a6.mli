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
val main: actions -> (frameElements list ref) -> (int ref) -> unit;;