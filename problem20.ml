(* P20 * Remove the K'th element from a list.

Example:
* (remove-at '(a b c d) 2)
(A C D)*)

let rec remove_at l i =
	match l with
		[]	-> []
	|	h::t	-> if ( i = 1 ) then t else h::( remove_at t ( i - 1 ) )
;;

if ( ( remove_at ["a";"b";"c";"d"] 2 ) = ["a";"c";"d"] )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;