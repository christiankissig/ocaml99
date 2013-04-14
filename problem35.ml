(*# P35 (**) Determine the prime factors of a given positive integer.
# 
# Construct a flat list containing the prime factors in ascending order.
# Example:
# * (prime-factors 315)
# (3 3 5 7)*)

let prime_factors n =
	let rec iterate i n =
		if ( i * i > n )
		then [n]
		else
			if ( ( n mod i ) = 0 )
			then i :: ( iterate i ( n / i ) )
			else iterate ( i + 1 ) n
	in
		iterate 2 n
;;

if ( ( prime_factors 315 ) = [3;3;5;7] )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;
