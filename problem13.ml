(* # P13 (**) Run-length encoding of a list (direct solution). *)
(* #  *)
(* # Implement the so-called run-length encoding data compression method directly. *)
(* # I.e. don't explicitly create the sublists containing the duplicates, as in *)
(* # problem P09, but only count them. As in problem P11, simplify the result list *)
(* # by replacing the singleton lists (1 X) by X. *)
(* #  *)
(* # Example: *)
(* # * (encode-direct '(a a a a b c c a a d e e e e)) *)
(* # ((4 A) B (2 C) (2 A) D (4 E)) *)

let encode_direct l = 
	let rec encode2 l x n a =
		match l with
			[]  	-> List.append a [(n,x)]
		|	h :: t	->
				if ( h = x )
				then ( encode2 t x ( n + 1 ) a )
				else ( encode2 t h 1 ( List.append a [(n,x)] ) )
	in
		match l with
			[]	-> []
		|	h::t	-> ( encode2 t h 1 [] )
;;

if ( ( encode_direct ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] ) = [(4,"a");(1,"b");(2,"c");(2,"a");(1,"d");(4,"e")] )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;