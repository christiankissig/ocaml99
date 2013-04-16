(* 
 * Problem 03: Find the k-th Element of a List 
 *)

let element_at l k = List.nth l ( k - 1 );;

(*
 * Solution using recursion
 *)
exception Empty_list;;

let rec element_at_2 l k =
    match l with
        [] -> raise Empty_list
    | h::t -> if k = 1 then h else element_at_2 t ( k - 1 )
;;

(*
 * This solution is due to Bart van Deenen. Thanks!
 *)
let rec element_at_3 l k =
    match (l,k) with
        ([], _)     -> raise Empty_list
    | (x :: _, 1)   -> x
    | (h :: t, _)   -> element_at_3 t (k-1)
;;

(*
 * Test
 *)
let test l n = Printf.printf "%s\n" ( element_at_3 l n );;

test ["a";"b";"c";"d"] 3;;
