(*P25 * Generate a random permutation of the elements of a list.

Example:
* (rnd-permu '(a b c d e f))
(B A D C E F)

Hint: Use the solution of problem P23.*)

let rec remove_ith l i =
	match l with
		[]	-> []
	|	h::t	-> if ( i = 0 ) then t else h::(remove_ith t ( i - 1 ) )
;;

let rec rnd_permu l =
	if ( l = [] )
	then []
	else
		let i = ( Random.int ( List.length l ) )in
		( List.nth l i )::( rnd_permu ( remove_ith l i ) )
;;

Random.self_init ();;
List.iter ( Printf.printf "%s " ) ( rnd_permu ["a";"b";"c";"d";"e";"f"] );;
Printf.printf "\n";;