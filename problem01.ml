(* Problem 01: Find the last element of a list *)

exception Empty_list;;

let my_last l = ( List.nth l ((List.length l) -1 ) );;

let rec my_last_2 l =
    match l with
        []  -> raise Empty_list
    |   h::[]   -> h
    |   h::t    -> my_last_2 t
;;

(** test **)
Printf.printf "%s\n" ( my_last ["a";"b";"c";"d"] );;
