(*P21 * Insert an element at a given position into a list.

Example:
* (insert-at 'alfa '(a b c d) 2)
(A ALFA B C D)*)

let rec insert_at x l i =
	if ( i = 1 ) then x::l else
	match l with
		[]	-> []
	|	h::t	-> h::( insert_at x t ( i - 1 ) )
;;

if ( ( insert_at "alfa" ["a";"b";"c";"d"] 2 ) = ["a";"alfa";"b";"c";"d"] )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;