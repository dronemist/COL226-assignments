(* Assignment 0: a BigInt package*)
open signature_a0
  module A0 : BigInt = struct
  (* Representational invariant of the elements of the int list:
    - the elements of the int list are between 0 and 9
    - presented most significant digit first
    - there are no unnecessary leading zeros. *)

  (* Arithmetic operations:  *)
  (* Addition *)
  type sign = Neg | NonNeg;;
  type bigint = sign * int list;;
  (*converting an integer to a list*)
  let rec posIntToList c = if c = 0 then 
                            []
                       else 
                          (posIntToList (c/10)) @ [c mod 10]   ;; 
  (* Conversion functions from OCaml int to bigint. *)  
  let mk_big num = if num < 0 then 
                    (Neg,posIntToList num)
                   else 
                    (NonNeg,posIntToList num);; 

  let bitAddc a b c = if a+b+c>=10 then
  					  	let sum = a+b+c in 
  					  		(sum/10,sum mod 10)
  					  else 
  					  	(0,a+b+c);;	
  exception IllFormedBigInt;;	
  (*carryFor function has been written for a reversed list i.e first digit is least significant*)	
  let rec carryFor l1 c = match l1 with
  					  [] -> if c=0 then []
  					  		else [c]
  					 |x::xs -> let (carry,sum) = bitAddc x c 0 in
  					  			if carry>0 then (sum :: (carryFor xs carry))
  					  			else  (sum::xs);;
  (*This add function takes number with least significant as the first digit and returns a list with least significant as the first digit*)
  let rec addRev l1 l2 c = match l1 with 
						[] -> carryFor l2 c
					|   x::xs -> (  match l2 with 
										[]-> carryFor l1 c
									|   y::ys -> let (carry,sum) = bitAddc x y c in
													sum::addRev xs ys carry 
								 );;
  (*addition of two numbers with same sign*)				
  let addPos l1 l2 = List.rev ( addRev (List.rev l1) (List.rev l2) 0 );; 
  let bitSubb x y b = if x-y-b < 0 then
  						let dif = (10+x) - y - b in
  							(1,dif)
  					  else 
  					  	(0,x-y-b);;	

  let rec borrowFor l1 b = match l1 with
  					   [] -> []
  					| [x] -> if x-b > 0 then [x-b]
  							 else []	      
  					| x::xs -> let (borrow,difference) = bitSubb x b 0 in 
  								if borrow = 0 then difference::xs
  								else difference::(borrowFor xs borrow);;  
  (*the function subRev assumes that value in l1 is greater than that in l2*)
  let rec subRev l1 l2 b = match l1 with
  							  [] -> []
  							| x::xs ->(	match l2 with
			  							| [] -> borrowFor l1 b
			  							| y::ys -> let (borrow,difference) = bitSubb x y b in
			  											difference::subRev xs ys borrow 
			  						  );;
  let subPos l1 l2 = List.rev ( subRev (List.rev l1) (List.rev l2) 0 );;		  						   													  	
  (* Multiplication *)
  val mult: bigint -> bigint -> bigint
  (* Subtraction *)
  val sub: bigint -> bigint -> bigint
  (* Quotient *)
  val div: bigint -> bigint -> bigint
  (* Remainder *)
  val rem: bigint -> bigint -> bigint
  (* Unary negation *)
  val minus: bigint -> bigint
  (* Absolute value *)
  val abs: bigint -> bigint

  (* Comparison operations:  *)
  (* Equal *)
  val eq: bigint -> bigint -> bool
  (* Greater_than. *)
  val gt:  bigint -> bigint -> bool
  (* Less_than. *)
  val lt:  bigint -> bigint -> bool
  (* Great_or_equal. *)
  val geq:  bigint -> bigint -> bool
  (* Less_or_equal.  *)
  val leq:  bigint -> bigint -> bool

  (* Functions to present the result in the form of a string. *)
  val print_num: bigint -> string 
end

(* Instructions for submission:
#1 Your submission should have one file named structure_a0.ml which implements the interface as below.
  open Signature_a0
  module A0 : BigInt = struct
   (* Your code goes here *)
  end

#2 Your code won't compile if all the functions declared in the signature_a0.mli are not implemented in the structure_a0.ml. In case you decide not to implement a function, but want your code to compile and run.
 - Mention this in the comments
 - Add "exception Not_implemented" in the structure_a0 (NOT signature_a0.mli)
 - Provide a dummy implementation (for those functions which you aren't implementing) which raises an exception Not_implemented
E.g.: In case you aren't implementing add
  exception Not_implemented
  let add n1 n2 = raise Not_implemented

#3 Keep signature_a0.mli and structure_a0.ml in the same folder
 A. Compile the signature file as
    ocamlc signature_a0.mli
    This creates a .cmi files
 B. Use the top level to test your code
    #use "structure_a0.ml";;
    open A0;;
    add (mk_big 5) (mk_big 10);;

#4 Grading criteria:
- Correctness
- Efficiency
- Code readability, comments etc

*)
