(*P23 ** Extract a given number of randomly selected elements from a list.

The selected items shall be returned in a list.
Example:
* (rnd-select '(a b c d e f g h) 3)
(E D A)

Hint: Use the built-in random number generator and the result of problem P20.*)

let rec remove_at l i =
	match l with
		[]	-> []
	|	h::t	-> if ( i = 1 ) then t else h::( remove_at t ( i - 1 ) )
;;

let rec rnd_select l n =
	if ( ( n < 1 ) || ( l = [] ) ) then [] else
	let i = ( Random.int ( List.length l ) ) in
	( List.nth l i )::( rnd_select ( remove_at l ( i + 1 ) ) ( n - 1 ) ) 
;;

Random.self_init () ;;
List.map ( fun x -> ( Printf.printf "%s " x ) ) ( rnd_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3 );;
Printf.printf "\n" ;;