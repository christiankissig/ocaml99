(* Problem 02: Find the last but one element of a list *)

exception Empty_list;;

let last_but_one l = ( List.nth l ((List.length l) - 2) );;

let rec last_but_one_2 l =
        match l with
                []      -> raise Empty_list
        |       h::[]   -> raise Empty_list
        |       h1::h2::[]      -> h1
        |       h1::h2::t       -> last_but_one_2 ( h2::t )
;;

(** test **)
Printf.printf "%s\n" ( last_but_one ["a";"b";"c";"d"] );;
