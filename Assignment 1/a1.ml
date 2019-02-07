open A0	
	(* abstract syntax  *)
	type  exptree =  N of int
    | Plus of exptree * exptree
    | Minus of exptree * exptree
    | Mult of exptree * exptree
    | Div of exptree * exptree
    | Rem of exptree * exptree
    | Nega of exptree 
    | Abs of exptree;;

    (* opcodes of the stack machine *)
    type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS;;

    let rec eval t = match t with
    | N(x) -> x
    | Plus(t1,t2) -> (eval t1) + (eval t2)
    | Minus(t1,t2) -> (eval t1) - (eval t2)
    | Mult(t1,t2) -> (eval t1) * (eval t2)
    | Div(t1,t2) -> (eval t1) / (eval t2)
    | Rem(t1,t2) -> (eval t1) mod (eval t2)
    | Nega(t1) -> (-(eval t1))
    | Abs(t1) -> if (eval t1) < 0 then (-(eval t1))
    			 else (eval t1);;
	(* exception to check if the stack is correctly formed *)    
    exception IllFormedStack;;
    (* Tail recursive function to evaluate the opcode list using a stack.
       I have assumed that opcList is complete ie it is correctly formed when opcode list is matched with an empty list. 
       If opcode list is empty I return the value in the stack else if the stack is not completely empty I raise an exception.
 	 *)
 	 (* as the left element is pushed first, I have done bin(z2,z1) for all the binary operators *)
    let rec stackmc l1 opcList = match opcList with
    | [] -> ( match l1 with	
	    	| [] -> mk_big 0
		    | x::xs -> x
			)
    | x::xs -> ( match x with
			    | CONST (y) -> stackmc (y::l1) (xs)
			    | PLUS -> (	(* The list should have atleast two elements if we match a binary operator *)
			    			match l1 with
			    			| z1::z2::zs -> stackmc ((add z2 z1)::zs) xs
			    			| _ -> raise IllFormedStack
			    		)	
			    | MINUS ->  (	match l1 with
			    			| z1::z2::zs -> stackmc ((sub z2 z1)::zs) xs
			    			| _ -> raise IllFormedStack
			    		)
			    | TIMES ->  (	match l1 with
			    			| z1::z2::zs -> stackmc ((mult z2 z1)::zs) xs
			    			| _ -> raise IllFormedStack
			    		)
			    | DIV ->	(	match l1 with
			    			| z1::z2::zs -> stackmc ((div z2 z1)::zs) xs
			    			| _ -> raise IllFormedStack
			    		)
			    | REM ->	(	match l1 with
			    			| z1::z2::zs -> stackmc ((rem z2 z1)::zs) xs
			    			| _ -> raise IllFormedStack
			    		) 
			    | UNARYMINUS ->	( (* The list should have atleast an element if we match a unary operator *)	
			    			match l1 with
			    			| z1::zs -> stackmc ((minus z1)::zs) xs
			    			| _ -> raise IllFormedStack
			    		)
		    	| ABS -> (	match l1 with
			    			| z1::zs -> stackmc ((abs z1)::zs) xs
			    			| _ -> raise IllFormedStack
			    		)
			);;

    (* Postorder Traversal *)
    (* Recursive function to find the postorder traversal of a tree *)
    let rec compile tree = match tree with
    | N(x) -> [CONST(mk_big x)]
    | Plus(t1,t2) ->  (compile t1) @ (compile t2) @ [PLUS]
    | Minus(t1,t2) ->  (compile t1) @ (compile t2) @ [MINUS]
    | Mult(t1,t2) ->  (compile t1) @ (compile t2) @ [TIMES]
    | Div(t1,t2) ->  (compile t1) @ (compile t2) @ [DIV]
    | Rem(t1,t2) ->  (compile t1) @ (compile t2) @ [REM]
    | Nega(t1) ->  (compile t1) @ [UNARYMINUS]
    | Abs(t1) ->  (compile t1) @ [ABS]