(*# P34 (**) Calculate Euler's totient function phi(m).
# 
# Euler's so-called totient function phi(m) is defined as the number of positive
# integers r (1 <= r < m) that are coprime to m.
# 
# Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
# 
# * (totient-phi 10)
# 4
# 
# Find out what the value of phi(m) is if m is a prime number. Euler's totient
# function plays an important role in one of the most widely used public key
# cryptography methods (RSA). In this exercise you should use the most primitive
# method to calculate this function (there are smarter ways that we shall discuss
# later).*)

let rec gcd a b =
	if ( b = 0 )
	then a
	else gcd b ( a mod b )
;;

let totient n =
	let rec iterate i =
		if ( i < 1 )
		then 0
		else
			( if ( ( gcd i n ) = 1 ) then 1 else 0 ) +
			( iterate ( i - 1 ) )
	in
		( iterate ( n - 1 ) )
;;

if ( ( totient 10 ) = 4 )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;