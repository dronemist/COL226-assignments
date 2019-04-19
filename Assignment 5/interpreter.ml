open A4;;
open A5;;
open A3;;
open A2;;
exception Incorrect_execution;;
(* function for parsing the expression *)
let exp_parser s = A3.exp_parser A2.read (Lexing.from_string s);;
(* function for extracting the answer from Closure i.e used for krivine *)
let ansFromClosure cl = match cl with
| VClosure(a,gamma) -> a
| _ -> raise Incorrect_execution;;
(* this extracts answer from Answer closure i.e used for secd *)
let ansFromAnswerclosure cl = match cl with
| Ansclos(a,gamma) -> a;;
(* Type checker *)
let typeChecker e = try(let t1 = (getType [] e) in true)
                   with Type_mismatch -> false     
let cbn s = let e = (exp_parser s) in ( if (typeChecker e) then (ansFromClosure (krivine (Closure(e,[])) []))
            else raise Type_mismatch)                                            
let cbv s = let e = (exp_parser s) in ( if (typeChecker e) then (ansFromAnswerclosure (secdmc [] [] (compile e) []))
            else raise Type_mismatch)    