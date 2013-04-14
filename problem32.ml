(*# P32 (**) Determine the greatest common divisor of two positive integer numbers.
# 
# Use Euclid's algorithm.
# Example:
# * (gcd 36 63)
# 9*)

let rec gcd a b =
	if ( b = 0 )
	then a
	else gcd b ( a mod b )
;;

if ( ( gcd 36 63 ) = 9 )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;