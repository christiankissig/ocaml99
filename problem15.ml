(* Replicate the elements of a list a given number of times. *)

let rec repli l n =
        let rec prep e n l =
                if ( n = 0 )
                then l
                else e::( prep e (n-1) l )
        in
        match l with
        [] -> []
        | h::t -> prep h n (repli t n)
;;

(* test *)
let rec print_list l =
    match l with
    [] -> ()
    | h::t -> Printf.printf "%s" h; print_list t
in
        print_list ( repli ["a";"b";"c"] 3 ); Printf.printf "\n";;
