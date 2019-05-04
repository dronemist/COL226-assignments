open A6
exception End_program;;
(* current stack pointer *)
let currPointer = ref 0;;
(* Initial stack *)
let stack = ref ([(Name("main"));LocalVals([("a",Tint,NumVal(0));("b",Tint,NumVal(0));("c",Tint,NumVal(0))])]);;

let rec inter x =
    Printf.printf "==> ";flush stdout;
    let lexbuf = Lexing.from_channel stdin in
    try 
        while true do
        (* try *)
            let result = Parser.act_parser Lexer.read lexbuf in
            A6.main result stack currPointer; 
            if (List.length !stack = 0) then raise End_program;
            Printf.printf "\n==> "; 
            flush stdout;
        done 
    with 
            End_program->
                    exit 0
            | Lexer.Exit ->
                    exit 0;
            | Parsing.Parse_error -> 
                    Printf.printf "Please enter a correct action \n";
                    flush stdout;    
                    inter 2;
            | Wrong_arguments -> 
                    Printf.printf "Incorrect arguments provided \n";
                    flush stdout;    
                    inter 2;
            | Cant_call ->
                    Printf.printf "Procedure can't be called \n";
                    flush stdout;    
                    inter 2;
            | Variable_cant_be_called -> 
                    Printf.printf "Variable not accessible \n";
                    flush stdout;    
                    inter 2;
            | Not_found -> 
                    Printf.printf "Not Found \n";
                    flush stdout;    
                    inter 2;    
            | Type_mismatch_in_assignment -> 
                    Printf.printf "LHS and RHS are not of same type  \n";
                    flush stdout;    
                    inter 2;
            | Variable_cant_be_accessed ->
                    Printf.printf "Variable not accessible \n";
                    flush stdout;    
                    inter 2;                        
;;

let _ = inter 2;;
