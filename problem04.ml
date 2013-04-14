(* Find the Number of Elements of a List *)

let my_length l = List.length l;;

let rec my_length_2 l =
        match l with
                []      -> 0
        |       h::t    -> 1 + ( my_length_2 t );;

(** test **)
Printf.printf "%d\n" ( my_length ["a";"b";"c";"d"] );;
