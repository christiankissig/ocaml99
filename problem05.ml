(* Reverse a list. *)

let my_reverse l = List.rev l;;

let rec my_reverse_2 l =
        match l with 
                []      -> []
        | h::t          -> ( ( my_reverse_2 t ) @ [h] )
;;

(* This is solution has been contributed by Nick Fishman *)
let my_reverse_3 = List.fold_left (fun a x -> x::a) [];;

(* test *)
let rec print_list l =
        match l with
                [] -> ()
        | ( h :: t ) -> Printf.printf "%s" h; print_list t
in
        print_list ( my_reverse ["a";"b";"c";"d"] );
        Printf.printf "\n"
;;


