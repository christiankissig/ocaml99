(* Drop every N'th element from a list. *)

let drop l n =
        let rec drop_2 l i =
                match l with
                [] -> []
                | h::t -> if ( i=1 ) 
                        then (drop_2 t n)
                        else h::( drop_2 t (i-1) )
        in
        drop_2 l n;;

(* test *)
let rec print_list l =
    match l with
    [] -> ()
    | h::t -> Printf.printf "%s" h; print_list t
in
print_list ( drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"k"] 3 ); Printf.printf "\n";;
