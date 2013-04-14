(* Extract a slice from a list. *)
let slice l b e =
        let rec slice_2 l b e f =
                match l with
                [] -> f
                | h::t -> if (b<2)
                        then
                                if (e>0)
                                then ( slice_2 t b (e-1) (f@[h]) )
                                else f
                        else
                                ( slice_2 t (b-1) (e-1) f )
        in
        slice_2 l b e [];;

(* test *)
let rec print_list l =
    match l with
    [] -> ()
    | h::t -> Printf.printf "%s" h; print_list t
in
        print_list ( slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"k"] 3 7 );
        Printf.printf "\n";; 
