(* Split a list into two parts; the length of the first part is given. *)
let split l n =
        let rec split_2 l i f =
                match l with
                [] -> (f,[])
                | h::t -> if (i=0)
                        then (f,t)
                        else ( split_2 t (i-1) (f@[h]) )
        in
        split_2 l n [];;

(* test *)
let rec print_list l =
    match l with
    [] -> ()
    | h::t -> Printf.printf "%s" h; print_list t
in
        match ( split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"k"] 3 ) with 
        (f,t) -> print_list f; Printf.printf "\n";
                print_list t; Printf.printf "\n";; 
