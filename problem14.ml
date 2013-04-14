(* Duplicate the elements of a list. *)

let rec dupli l =
    match l with
    [] -> []
    | h::t -> h::(h::(dupli t));;

(* test *)
let rec print_list l =
    match l with
    [] -> ()
    | h::t -> Printf.printf "%s" h; print_list t
in
print_list ( dupli ["a";"b";"c";"d"] ); Printf.printf "\n";;
