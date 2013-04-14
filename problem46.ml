(*# P46 (**) Truth tables for logical expressions.
# 
# Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
# logical equivalence) which succeed or fail according to the result of their
# respective operations; e.g. and(A,B) will succeed, if and only if both A and B
# succeed. Note that A and B can be Prolog goals (not only the constants true and
# fail).
# 
# A logical expression in two variables can then be written in prefix notation,
# as in the following example: and(or(A,B),nand(A,B)).
# 
# Now, write a predicate table/3 which prints the truth table of a given logical
# expression in two variables.
# 
# Example:
# * table(A,B,and(A,or(A,B))).
# true true true
# true fail true
# fail true fail
# fail fail fail*)

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

(* evaluate a formula f given a valuation of a and b *)
let rec evaluate f a b =
	let xor x1 x2 = ( ( x1 || x2 ) && ( not ( x1 && x2 ) ) ) in
	let impl x1 x2 = ( ( not x1 ) || x2 ) in 
	let equ x1 x2 = ( ( impl x1 x2 ) && ( impl x2 x1 ) ) in

	match f with
		Var "a"		-> a
	|	Var "b"		-> b
	|	Var x		-> false
	|	And (f1,f2)	-> ( evaluate f1 a b ) && ( evaluate f2 a b )
	|	Or (f1,f2)	-> ( evaluate f1 a b ) || ( evaluate f2 a b )
	|	Nand (f1,f2) 	-> not ( ( evaluate f1 a b ) && ( evaluate f2 a b ) )
	|	Nor (f1,f2) 	-> not ( ( evaluate f1 a b ) || ( evaluate f2 a b ) )
	|	Xor (f1,f2) 	-> xor ( evaluate f1 a b ) ( evaluate f2 a b )
	|	Impl (f1,f2)	-> impl ( evaluate f1 a b ) ( evaluate f2 a b )
	|	Equ (f1,f2)	-> equ ( evaluate f1 a b ) ( evaluate f2 a b )
;;

let table f =
	let print_bool b = if b then ( Printf.printf "true " ) else ( Printf.printf "fail " ) in
	let print_row a b =
		print_bool a ;
		print_bool b ;
		print_bool ( evaluate f a b );
		Printf.printf "\n" 
	in
		print_row true true;
		print_row true false;
		print_row false true;
		print_row false false
;;

table (And(Var "a",Or(Var "a",Var "b")))