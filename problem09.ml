(* Pack consecutive duplicates of list elements into sublists. If a list
contains repeated elements they should be placed in separate sublists. *)

let pack l =
        let rec pack_2 l s e =
                match l with
                [] -> [s]
                | h::t -> 
                                if (h=e)
                                then ( pack_2 t (h::s) e )
                                else s::( pack_2 t [h] h )
        in
                match l with
                [] -> []
                | h::t -> pack_2 t [h] h
;;

(* test *)
let rec print_list l =
        match l with
                [] -> ()
        | ( h :: t ) -> Printf.printf "%s" h; print_list t
in let rec print_lol l =
        match l with
        [] -> ()
        | h::t -> print_list h ; Printf.printf " "; print_lol t
in
        print_lol
        ( pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] );
        Printf.printf "\n"
;;

