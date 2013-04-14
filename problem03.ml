(* Problem 03: Find the k-th Element of a List *)

exception Empty_list;;

let element_at l k = ( List.nth l ( k - 1 ) );;

let rec element_at_2 l k =
        match l with
                []      -> raise Empty_list
        |       h::t    -> if ( k = 1 ) then h else ( element_at_2 t ( k - 1 ) )
;;
                

(** test **)
Printf.printf "%s\n" ( element_at ["a";"b";"c";"d"] 3 );;
