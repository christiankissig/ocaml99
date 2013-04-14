(*P39 * A list of prime numbers.

Given a range of integers by its lower and upper limit, construct a list of all
prime numbers in that range.*)

let primes_up_to n =
	let rec primes_up_to i n primes =
		if ( i < 2 ) then ( primes_up_to 2 n primes ) else		(* start at 2 *)
 		if ( i > n ) then primes else					(* finish at n *)
		if ( List.exists ( fun p -> ( ( i mod p ) = 0 ) ) primes )	(* check relative primality against smaller primes *)
		then ( primes_up_to ( i + 1 ) n primes )
		else ( primes_up_to ( i + 1 ) n ( i::primes ) )
	in
		primes_up_to 2 n []
;;

let primes l u =
	( List.fold_right							(* filter out all primes lower than the lower bound *)
		( fun p primes -> ( if ( p < l ) then primes else ( p::primes ) ) )
		( primes_up_to 2 u [] )
		[] )
;;

( List.iter ( fun p -> ( Printf.printf "%d " p ) ) ( primes 10 100 ) );;