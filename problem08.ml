(* Eliminate consecutive duplicates of list elements. *)

let compress l =
        let rec compress_2 l e =
                match l with
                        []      -> [e]
                | h::t          ->
                                        if ( h = e ) 
                                        then ( compress_2 t e )
                                        else e::( compress_2 t h )
        in
                match l with
                        []      -> []
                |       h::t    -> compress_2 t h
;;

(* test *)
let rec print_list l =
        match l with
                [] -> ()
        | ( h :: t ) -> Printf.printf "%s" h; print_list t
in
        print_list 
        ( compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] );
        Printf.printf "\n"
;;
