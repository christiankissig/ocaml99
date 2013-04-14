(* # P12 ** Decode a run-length encoded list. *)
(* #  *)
(* # Given a run-length code list generated as specified in problem P11. Construct *)
(* # its uncompressed version. *)

let decode l =
	let rec decode_block (n,x) =
		if ( n = 0 ) then [] else x :: ( decode_block ( n - 1, x ) )
	in
		List.flatten ( List.map decode_block l ) ;;

if ( ( decode  [(4,"a");(1,"b");(2,"c");(2,"a");(1,"d");(4, "e")] ) = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;
