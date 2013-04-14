(*# P36 (**) Determine the prime factors of a given positive integer (2).
# 
# Construct a list containing the prime factors and their multiplicity.
# Example:
# * (prime-factors-mult 315)
# ((3 2) (5 1) (7 1))
# 
# Hint: The problem is similar to problem P13.*)

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


let prime_factors_mult n = encode_direct ( prime_factors n );;

if ( ( prime_factors_mult 315 ) = [(2,3);(1,5);(1,7)] )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;