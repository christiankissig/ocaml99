(*# P41 (**) A list of Goldbach compositions.
# 
# Given a range of integers by its lower and upper limit, print a list of all
# even numbers and their Goldbach composition.
# 
# Example:
# * (goldbach-list 9 20)
# 10 = 3 + 7
# 12 = 5 + 7
# 14 = 3 + 11
# 16 = 3 + 13
# 18 = 5 + 13
# 20 = 3 + 17
# 
# In most cases, if an even number is written as the sum of two prime numbers,
# one of them is very small. Very rarely, the primes are both bigger than say 50.
# Try to find out how many such cases there are in the range 2..3000.
# 
# Example (for a print limit of 50):
# * (goldbach-list 1 2000 50)
# 992 = 73 + 919
# 1382 = 61 + 1321
# 1856 = 67 + 1789
# 1928 = 61 + 1867
# 
# Logic and Codes*)

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

let goldbach_list l u = 
	let primes = ( primes_up_to u ) in
	let rec search primes n =
		match primes with
		|	[]	-> (0,0)
		|	h::t	-> 
				if ( h < ( n / 2 ) ) then (0,0)  else		(* we need to check the first ( upper ) half only *)
				if ( List.mem ( n - h ) primes )		(* find the complementary prime in primes *)
				then ( h, ( n - h ) )
				else ( search t n )
	in
	let rec goldbach_list l u =
		if ( not ( l > u ) ) then					(* stop when l > u *)
		if ( ( l mod 2 ) = 1 ) then ( goldbach_list ( l + 1 ) u ) else	(* even numbers only *)
		match ( search primes l ) with (x,y) -> ( Printf.printf "%d = %d + %d\n" l x y ) ;
		( goldbach_list ( l + 2 ) u )
	in
		goldbach_list l u
;;

( goldbach_list 1 2000 );;