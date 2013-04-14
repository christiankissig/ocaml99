(*# P49 (**) Gray code.
# 
# An n-bit Gray code is a sequence of n-bit strings constructed according to
# certain rules. For example,
# 
# n = 1: C(1) = ['0','1'].
# n = 2: C(2) = ['00','01','11','10'].
# n = 3: C(3) = ['000','001','011','010',Â´110Â´,Â´111Â´,Â´101Â´,Â´100Â´].
# 
# Find out the construction rules and write a predicate with the following
# specification:
# 
# % gray(N,C) :- C is the N-bit Gray code
# 
# Can you apply the method of "result caching" in order to make the predicate
# more efficient, when it is to be used repeatedly?*)

let rec gray n =
	if n < 2
	then ["0";"1"]
	else
		let c = gray ( n - 1) in
			( List.map ( fun x -> "0" ^ x ) c ) @
			( List.map ( fun x -> "1" ^ x ) ( List.rev c ) )
;;

if ( ( gray 3 ) = ["000";"001";"011";"010";"110";"111";"101";"100"] )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;