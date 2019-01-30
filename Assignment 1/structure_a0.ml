(* Assignment 0: a BigInt package*)
  open Signature_a0
  module A0 : BigInt = struct
  (*converting an integer to a list*)
    type sign = Neg | NonNeg;;
    type bigint = sign * int list;;
    (*converting int to list*)
    let rec posIntToList c = if c = 0 then 
                            []
                       else 
                          (posIntToList (c/10)) @ [c mod 10]   ;; 
  (* Conversion functions from OCaml int to bigint. *)  
    let mk_big num = if num < 0 then 
                    (Neg,posIntToList (-num))
                   else 
                    (NonNeg,posIntToList num);; 
  (* Function to present the result in the form of a string. *)
    let rec print_list l1 = match l1 with
    | [] -> ""
    | x::xs -> (string_of_int x)^(print_list xs);;


    let print_num num = match num with
    | (NonNeg,l1) -> if (List.length l1) >=1 then print_list l1
                      else "0"
    | (Neg,l1) -> ("-")^(print_list l1);;                  
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
             |x::xs -> let (carry,sum) = (bitAddc x c 0) in
                    if carry>0 then (sum :: (carryFor xs carry))
                    else  (sum::xs);;
  (*This add function takes number with least significant as the first digit and returns a list with least significant as the first digit*)
    let rec addRev l1 l2 c = match l1 with 
            [] -> carryFor l2 c
          |   x::xs -> (  match l2 with 
                    []-> carryFor l1 c
                  |   y::ys -> let (carry,sum) = (bitAddc x y c) in
                          sum::addRev xs ys carry 
                 );;
  (* addition of two numbers with same sign *)        
  let addPos l1 l2 = List.rev ( addRev (List.rev l1) (List.rev l2) 0 );; 
  (* bit subtraction *)
  let bitSubb x y b = if x-y-b < 0 then
              let dif = (10+x) - y - b in
                (1,dif)
              else 
                (0,x-y-b);; 
  (* borrow forward *)
  let rec borrowFor l1 b = match l1 with
               [] -> []
            | [x] -> if x-b > 0 then [x-b]
                 else []        
            | x::xs -> let (borrow,difference) = (bitSubb x b 0) in 
                  if borrow = 0 then difference::xs
                  else difference::(borrowFor xs borrow);;  
  (*the function subRev assumes that value in l1 is greater than that in l2*)
    let rec subRev l1 l2 b = match l1 with
                  [] -> []
                | x::xs ->( match l2 with
                      | [] -> borrowFor l1 b
                      | y::ys -> let (borrow,difference) = (bitSubb x y b) in
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
                          | y::ys -> if (List.length l1) = (List.length l2) then 
                                         if x = y then compList xs ys
                                         else if x > y then 1 
                                         else (-1)
                                     else if (List.length l1) > (List.length l2) then 1
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
  (*Subtraction*)
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
   (*here a is a single digit integer ie 0-9 and l1 is a reversed list*)  
   let rec multSingleDigit l1 a c = match l1 with
                 | [] -> if c = 0 then []
                       else [c]
                 | x::xs -> let mult = (x*a) + c in
                        (mult mod 10)::(multSingleDigit xs a (mult/10))  
                  ;;
  (*multiplication by 10*)                  
  let listShiftRight l1 = match l1 with
            | [] -> []
            | _ -> 0::l1;;

    (*multiply two reversed lists*)
  let rec multList res l1 l2 = match l1 with
              | [] -> res
              | x::xs -> multList (addRev res (multSingleDigit l2 x 0) 0) xs (listShiftRight l2)
              ;;
  let multPos l1 l2 = List.rev (multList [] (List.rev l1) (List.rev l2));;
  (*Multiplication*)
  let mult b1 b2 = match b1 with
          | (NonNeg,l1) -> ( match b2 with
                    | (NonNeg,l2) -> (NonNeg,(multPos l1 l2))
                    | (Neg,l2) -> (Neg,(multPos l1 l2))
                  )
          | (Neg,l1) -> ( match b2 with
                  | (NonNeg,l2) -> (Neg,(multPos l1 l2))
                  | (Neg,l2) -> (NonNeg,(multPos l1 l2))
                );;
  (* Quotient *)
  (*div list helper finds a single digit of the quotient*)
    exception DivisionByZero;;
    let rec divListHelper l1 l2 q = if (compList (List.rev l1) (List.rev l2)) = -1 then (q,l1)
                    else  divListHelper (subRev l1 l2 0) l2 (q+1)
                  ;;
    (* This function makes l2 equal to l1 by appending zeros *)              
    let rec mkEqual l1 l2 = if (List.length l1) > (List.length l2) then mkEqual l1 (0::l2)
                else l2;;   
    (* Here both l1 and l2 are reversed lists *)            
    let rec divList l1 l2 q bitsShifted = match l2 with
                | [] -> raise DivisionByZero 
                | _ -> ( match l1 with
                      | [] -> (0::q,[])
                      | _ ->  if (compList (List.rev l1) (List.rev l2)) = -1 then 
                            if bitsShifted!=0 then
                            divList l1 (List.tl l2) (0::q) (bitsShifted-1)
                            else
                            (0::q,l1)
                      else 
                        let (quotient,rem) = (divListHelper l1 l2 0) in
                          if bitsShifted > 0 then
                          divList (List.rev (rmLeadingZero(List.rev rem))) (List.tl l2) (quotient::q) (bitsShifted-1)
                          else
                          (quotient::q,rem)
                            
                    );;
    let divPos l1 l2 = match l2 with
    | [] -> raise DivisionByZero
    | _ -> if (compList l1 l2) = -1 then ([],l1)
           else 
            let (q,r) = (divList (List.rev l1) (mkEqual l1 (List.rev l2)) [] ((List.length l1)-(List.length l2))) in
                (rmLeadingZero(List.rev q),rmLeadingZero (List.rev r));;  
                             
  (*In division I have considerred that remainder has same sign as the divident*)
  (*Quotient*)
  let div b1 b2 = match b1 with
  | (Neg,l1) -> ( match b2 with
          | (Neg,l2) ->  
                  let (q,r) = (divPos l1 l2) in
                    (NonNeg,q)
          | (NonNeg,l2) -> let (q,r) = (divPos l1 l2) in
                    (match q with
                    | [] -> (NonNeg,q)
                    | _ -> (Neg,q)
                    )
          ) 
  | (NonNeg,l1) -> ( match b2 with
          | (Neg,l2) -> let (q,r) = (divPos l1 l2) in
                (match q with
                  | [] -> (NonNeg,q)
                  | _ -> (Neg,q)
                )
          | (NonNeg,l2) -> let (q,r) = (divPos l1 l2) in
                  (NonNeg,q)
          ) ;; 
  (* Remainder*)
    let rem b1 b2 = match b1 with
    | (Neg,l1) -> ( match b2 with
            | (_,l2) -> let (q,r) = (divPos l1 l2) in
                    (match r with
                    | [] -> (NonNeg,r)
                    | _ -> (Neg,r))
                  
          )
    | (NonNeg,l1) -> (  match b2 with
              | (_,l2) -> let (q,r) = (divPos l1 l2) in
                  (NonNeg,r)
             );;

  (* Unary negation *)
  let minus b1 = match b1 with
  | (Neg,l1) -> (NonNeg,l1)
  | (NonNeg,l1) -> (Neg,l1);;

  (* Absolute value *)
  let abs b1 = match b1 with
  |(_,l1)  -> (NonNeg,l1);;
  
  (* Comparison operations:  *)
  (* Equal *)
  let eq b1 b2 = match b1 with
  | (NonNeg,l1) -> (match b2 with
            | (NonNeg,l2) -> if (compList l1 l2) = 0 then true
                     else false 
            | (Neg,_) -> false
            )
  | (Neg,l1) ->   (match b2 with
            | (Neg,l2) -> if (compList l1 l2) = 0 then true
                     else false 
            | (NonNeg,_) -> false
            )
  (* Greater_than. *) 
  let gt b1 b2 = match b1 with 
    (NonNeg,l1) -> ( match b2 with
                    | (NonNeg,l2) -> if compList l1 l2 = 1 then true
                                    else false
                    | (Neg,_) -> true
                  )
    |(Neg,l1) -> ( match b2 with
                  | (NonNeg,_) -> false
                  | (Neg,l2) -> if compList l1 l2 = -1 then true
                                  else false
                  );;
  (*Less than*)
  let lt b1 b2 = if ((gt b1 b2) = true || (eq b1 b2) = true) then false
           else true;;      
  (* Great_or_equal. *)
  let geq b1 b2 = if (lt b1 b2) = true then false
            else true;; 
  (* Less_or_equal.  *)
  let leq b1 b2 = if (gt b1 b2) = true then false 
            else true;;     
end
