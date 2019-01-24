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
                    (Neg,posIntToList (-num))
                   else 
                    (NonNeg,posIntToList num);; 
  (*bit addition*)                
  let bitAddc a b c = if a+b+c>=10 then
  					  	let sum = a+b+c in 
  					  		(sum/10,sum mod 10)
  					  else 
  					  	(0,a+b+c);;	
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
			  											difference::(subRev xs ys borrow) 
			  						  );;
  (*remove leading zeros in list*)              
  let rec rmLeadingZero l1 = match l1 with
        | [] -> []
        | x::xs -> if x = 0 then rmLeadingZero xs
                    else l1;;              
  let subPos l1 l2 = rmLeadingZero (List.rev ( subRev (List.rev l1) (List.rev l2) 0 ));;

  (*This function return 1 if l1 > l2, 0 if l1 = l2, -1 if l1 < l2*)
  let rec compList l1 l2 = match l1 with
            [] ->( match l2 with
                      [] -> 0
                    | y::ys -> (-1)
                  )
            | x::xs -> ( match l2 with
                          [] -> 1 
                          | y::ys -> if List.length l1 = List.length l2 then 
                                         if x = y then compList xs ys
                                         else if x > y then 1 
                                         else (-1)
                                     else if List.length l1 > List.length l2 then 1
                                     else -1    
                      );; 
  (*addition*)        
  let add a b = match a with
      (NonNeg,la) -> ( match b with
                    | (NonNeg,lb) ->(NonNeg,addPos la lb)
                    | (Neg,lb)-> if (compList la lb) = -1 then (Neg,subPos lb la)
                                 else (NonNeg,subPos la lb)
                      )
      |(Neg,la) ->  ( match b with
                    | (NonNeg,lb) ->if (compList la lb) = 1 then (Neg,subPos la lb)
                                    else (NonNeg,subPos lb la)
                    | (Neg,lb)-> (Neg,addPos la lb)
                    );; 
  
  let sub a b = match a with 
      (NonNeg,la) -> ( match b with
                      (NonNeg,lb) -> if (compList la lb) = -1 then
                                        (Neg,(subPos lb la))
                                     else (NonNeg,(subPos la lb))   
                      |(Neg,lb) -> (NonNeg,addPos la lb)
                    )
     | (Neg,la) -> ( match b with
                      (NonNeg,lb) -> (Neg,addPos la lb)  
                      |(Neg,lb) -> if (compList la lb) = 1 then
                                        (Neg,(subPos la lb))
                                   else (NonNeg,(subPos lb la)) 
                  ) ;;

  (* (* Multiplication *)
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
  (* Greater_than. *) *)
  let gt b1 b2 = match b1 with 
            (NonNeg,l1) -> ( match b2 with
                             (NonNeg,l2) -> if compList l1 l2 = 1 then true
                                            else false
                            | (Neg,_) -> true
                          )
          |(Neg,l1) -> ( match b2 with
                             (NonNeg,_) -> false
                            | (Neg,l2) -> if compList l1 l2 = -1 then true
                                            else false
                          );; 
  (* (* Less_than. *)
  val lt:  bigint -> bigint -> bool
  (* Great_or_equal. *)
  val geq:  bigint -> bigint -> bool
  (* Less_or_equal.  *)
  val leq:  bigint -> bigint -> bool

  (* Functions to present the result in the form of a string. *)
  val print_num: bigint -> string  *)

