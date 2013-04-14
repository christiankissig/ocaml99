(*P40 ** Goldbach's conjecture.

Goldbach's conjecture says that every positive even number greater than 2 is
the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
famous facts in number theory that has not been proved to be correct in the
general case. It has been numerically confirmed up to very large numbers (much
larger than we can go with our Prolog system). Write a predicate to find the
two prime numbers that sum up to a given even integer.

Example:
* (goldbach 28)
(5 23)*)

(* lists all primes from 2 to n in descending order *)
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

let goldbach n = 
	let rec search primes =
		match primes with
		|	[]	-> (0,0)
		|	h::t	-> 
				if ( h < ( n / 2 ) ) then (0,0)  else		(* we need to check the first ( upper ) half only *)
				if ( List.mem ( n - h ) primes )		(* find the complementary prime in primes *)
				then ( h, ( n - h ) )
				else search t
	in
		search ( primes_up_to n )
;;

match ( goldbach 28 ) with (x,y) -> ( Printf.printf "%d = %d + %d\n" 28 x y ) ;;