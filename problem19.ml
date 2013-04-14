(* Rotate a list N places to the left. *)
let rotate l n =
    let len = List.length l in
    let n = ( ( ( n mod len ) + len ) mod len ) in
    let rec rotate l n =
        if (n=0)
        then l
        else match l with
            [] -> []
            | h::t -> rotate (t@[h]) (n-1)
    in
    rotate l n
;;

(* test *)
let rec print_list l =
    match l with
    [] -> ()
    | h::t -> Printf.printf "%s" h; print_list t
in
        print_list ( rotate ["a";"b";"c";"d";"e";"f";"g";"h"] (-2) );
        Printf.printf "\n";; 
