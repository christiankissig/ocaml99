(* 
 * P31 (**) Determine whether a given integer number is prime.
 *)

let is_prime n =
    if n < 2 then false else
    let rec iterate i =
        if i * i > n then true else
		if ( n mod i ) = 0 then false else iterate ( n + 1 )
	in
		iterate 2;;

(*
 * This is solution is due to Deniz Tohumcu. Thanks!
 *)
let is_prime_2 : int -> bool = fun(n) -> 
    if n = 1 then false else
    let son = ref true
    and k = ref 2 in
    while !son && !k * !k <= n do
        if n mod !k = 0 then son := false else son := true;
        k := !k + 1
    done; !son;;

(*
 * Test
 *)
let test_run n = Printf.printf "%d is prime: %B\n" n (is_prime n);;

test_run 1;;
test_run 2;;
test_run 7;;
