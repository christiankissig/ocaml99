(* Reverse a list. *)

let my_reverse l = List.rev l;;

let rec my_reverse_2 l =
    match l with
        []  -> []
    | h::t  -> ( my_reverse_2 t ) @ [h]
;;

(*
 * This solution is due to Nick Fishman. Thanks!
 *)
let my_reverse_3 = List.fold_left (fun a x -> x::a) [];;

(*
 * This solution is due to Bart van Deenen. Thanks!
 *
 * This solution is tail-recursive.
 *)
let my_reverse_4 l =
    let rec rev l2 acc =
        match l2 with
            []  -> acc
        | h::t  -> rev t (h::acc)
    in
        rev l []
;;

(* 
 * Test 
 *)
let rec print_list l =
    match l with
        []          -> ()
    | ( h :: t )    -> Printf.printf "%s" h; print_list t
;;

let test l =
    print_list (my_reverse l);
    Printf.printf "\n"
;;

test ["a";"b";"c";"d"];;

