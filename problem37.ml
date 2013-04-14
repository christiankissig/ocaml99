(*# P37 (**) Calculate Euler's totient function phi(m) (improved).
# 
# See problem P34 for the definition of Euler's totient function. If the list of
# the prime factors of a number m is known in the form of problem P36 then the
# function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2)
# (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given
# number m. Then phi(m) can be calculated with the following formula:
# 
# phi(m) = (p1 - 1) * p1 ** (m1 - 1) * (p2 - 1) * p2 ** (m2 - 1) * (p3 - 1) * p3 ** (m3 - 1) * ...
# 
# Note that a ** b stands for the b'th power of a.*)

let encode_direct l = 
	let rec encode2 l x n a =
		match l with
			[]  	-> List.append a [(n,x)]
		|	h :: t	->
				if ( h = x )
				then ( encode2 t x ( n + 1 ) a )
				else ( encode2 t h 1 ( List.append a [(n,x)] ) )
	in
		match l with
			[]	-> []
		|	h::t	-> ( encode2 t h 1 [] )
;;

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

let phi m =
	let exp n m = int_of_float ( ( float_of_int n ) ** ( float_of_int m ) ) in
	let rec sum l =
		match l with
			[]		-> 1
		|	(n,p)::t	-> ( ( p - 1 ) * ( exp p ( n - 1 ) ) ) * ( sum t )
	in
		sum ( encode_direct ( prime_factors m ) )
;;

if ( ( phi 10 ) = 4 )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;
