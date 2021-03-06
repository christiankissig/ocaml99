(* # P07 (**) Flatten a nested list structure. *)
(* #  *)
(* #  *)
(* # Transform a list, possibly holding lists as elements into a `flat' list by *)
(* # replacing each list with its elements (recursively). *)
(* #  *)
(* # Example: *)
(* # * (my-flatten '(a (b (c d) e))) *)
(* # (A B C D E) *)
(* #  *)
(* # Hint: Use the predefined functions list and append. *)
(*  *)
(* my $flatten = -> $x { $x.isa(Array) ?? ( map $flatten, $x ) !! $x };  *)
(* my @flattened = map $flatten, ('a', ['b', ['c', 'd', 'e']]); *)
(* is @flattened, <a b c d e>, 'We should be able to flatten lists'; *)

(* we got static typing ;) *)
let my_flatten l = List.flatten l;;

if ( ( my_flatten [["a";"b"];["c";"d"];["e"]] ) = ["a";"b";"c";"d";"e"] )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;