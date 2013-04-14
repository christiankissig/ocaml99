(*# P33 * Determine whether two positive integer numbers are coprime.
# 
# Two numbers are coprime if their greatest common divisor equals 1.
# Example:
# * (coprime 35 64)
# T*)

let rec gcd a b =
	if ( b = 0 )
	then a
	else gcd b ( a mod b )
;;

let coprime a b = ( ( gcd a b ) = 1 );;

if ( coprime a b )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;