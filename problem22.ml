(*P22 * Create a list containing all integers within a given range.

If first argument is smaller than second, produce a list in decreasing order.
Example:
* (range 4 9)
(4 5 6 7 8 9)*)

let rec range l u =
	if ( l > u )
	then []
	else l::( range ( l + 1 ) u )
;;

if ( ( range 4 9 ) = [4;5;6;7;8;9] )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;
