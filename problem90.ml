(* Eight queens problem
   This is a classical problem in computer science. The objective is to
   place eight queens on a chessboard so that no two queens are attacking
   each other; i.e., no two queens are in the same row, the same column,
   or on the same diagonal.

   Hint: Represent the positions of the queens as a list of numbers 1..N.
   Example: [4,2,7,3,6,8,5,1] means that the queen in the first column
   is in row 4, the queen in the second column is in row 2, etc.
   Use the generate-and-test paradigm. *)

let queens =
  let rec queens v xn c p =
    match v with
    [] -> if ( p = [] ) then [c] else []
    | yn::t ->
	if ( List.exists ( fun (x,y)->( abs (xn-x) = abs (yn-y) ) ) c )
	then 
	  ( queens t xn c (yn::p) )
	else 
	  ( queens (p@t) (xn+1) ((xn,yn)::c) [] )@
	  ( queens t xn c (yn::p) )	
  in
    ( queens [0;1;2;3;4;5;6;7] 0 [] [] )
;;

let rec print_lop l =
    match l with
    [] -> ()
    | (x,y)::t -> Printf.printf "(%d,%d) " x y; print_lop t
  in
let rec print_lol l =
    match l with
    [] -> ()
    | h::t -> print_lop h; Printf.printf "\n"; print_lol t
  in
    print_lol ( queens )
;;


  
  