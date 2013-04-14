(* Run-length encoding of a list. Use the result of problem P09 to implement the
 * so-called run-length encoding data compression method. Consecutive duplicates
 * of elements are encoded as lists (N E) where N is the number of duplicates of
 * the element E. *)

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

let encode l =
        let p = pack l
        in
                List.map (fun s -> ( List.length(s), List.hd s ) ) p
;;

(* test *)
let rec print_lop l =
        match l with
        [] -> ()
                | (n,e)::t -> Printf.printf "(%d,%s)" n e; print_lop t
in
        print_lop ( encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] );
        Printf.printf "\n";;
