(*# P31 (**) Determine whether a given integer number is prime.
# 
# Example:
# * (is-prime 7)
# T *)

let is_prime n =
    if ( n < 2 ) then false else
    let rec iterate i =
        if ( i * i > n  ) then true else
		if ( ( n mod i ) = 0 ) then false else iterate ( n + 1 )
	in
		iterate 2;;

if ( is_prime 37 ) 
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;
