(*# P48 (**) Truth tables for logical expressions (3).
# 
# Generalize problem P47 in such a way that the logical expression may contain
# any number of logical variables. Define table/2 in a way that table(List,Expr)
# prints the truth table for the expression Expr, which contains the logical
# variables enumerated in List.
# 
# Example:
# * table([A,B,C], A and (B or C) equ A and B or A and C).
# true true true true
# true true fail true
# true fail true true
# true fail fail true
# fail true true true
# fail true fail true
# fail fail true true
# fail fail fail true*)

type form = 
		Var of string
	|	And of form * form
	|	Or of form * form
	|	Nand of form * form
	|	Nor of form * form
	|	Xor of form * form
	|	Impl of form * form
	|	Equ of form * form
;;

(* evaluate a formula f given a valuation v *)
let rec evaluate f v =
	let xor x1 x2 = ( ( x1 || x2 ) && ( not ( x1 && x2 ) ) ) in
	let impl x1 x2 = ( ( not x1 ) || x2 ) in 
	let equ x1 x2 = ( ( impl x1 x2 ) && ( impl x2 x1 ) ) in

	match f with
		Var x		-> ( Hashtbl.find v x )
	|	And (f1,f2)	-> ( evaluate f1 v ) && ( evaluate f2 v )
	|	Or (f1,f2)	-> ( evaluate f1 v ) || ( evaluate f2 v )
	|	Nand (f1,f2) 	-> not ( ( evaluate f1 v ) && ( evaluate f2 v ) )
	|	Nor (f1,f2) 	-> not ( ( evaluate f1 v ) || ( evaluate f2 v ) )
	|	Xor (f1,f2) 	-> xor ( evaluate f1 v ) ( evaluate f2 v )
	|	Impl (f1,f2)	-> impl ( evaluate f1 v ) ( evaluate f2 v )
	|	Equ (f1,f2)	-> equ ( evaluate f1 v ) ( evaluate f2 v )
;;

(* iterate over all possible valuations v for variables in the list l and call for each valuation the hook h *)
let rec iterate v l h =
	(* given an ordered list of variables, a valuation of those variables can be conceived as a binary string. we iterate over all valuations as binary coded numbers. *)
	let carry x i = 
		if i
		then
			let d = ( Hashtbl.find v x ) in
			Hashtbl.replace v x ( not d ) ;	(* negate value of variable x *)
			d				(* carry over ( not d ) *)
		else 
			i				(* carry over i *)
	in
		h v;
		if ( not ( List.fold_right carry l true ) ) (* if true is returned, we have exceeded all possible valuations *)
		then iterate v l h;;

(* print a row in the truth table for formula f with variables from l amd a valuation v *)
let print_row f l v =
	let print_bool b = if b then ( Printf.printf "true " ) else ( Printf.printf "fail " ) in
	List.iter print_bool ( List.map ( Hashtbl.find v ) l );
	print_bool ( evaluate f v );
	( Printf.printf "\n" );;

(* generate truth table for formula f with variables from l *)
let table l f =
	let v = ( Hashtbl.create ( List.length l ) ) in
	List.iter ( fun x -> Hashtbl.add v x false ) l;
(* 	List.iter ( fun x -> print_string x; print_string " " ) l; *)
(* 	Printf.printf "f\n"; *)
	iterate v l ( print_row f l );;

table ["a";"b";"c"] (Equ(And(Var "a",Or(Var "b",Var "c")),Or(And(Var "a",Var "b"),And(Var "a",Var "c"))));;