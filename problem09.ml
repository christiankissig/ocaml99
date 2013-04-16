(* 
 * Pack consecutive duplicates of list elements into sublists. 
 * If a list contains repeated elements they should be placed 
 * in separate sublists. 
 *)

let pack l =
    let rec pack_2 l s e =
        match l with
            [] -> [s]
        | h::t -> 
            if h = e
            then ( pack_2 t (h::s) e )
            else s::( pack_2 t [h] h )
    in
        match l with
            [] -> []
        | h::t -> pack_2 t [h] h
;;

(*
 * This solution is due to Bart van Deenen. Thanks!
 *
 * g1 just splits of the first group of identical elements 
 * of a list and returns a tuple of that group and the remainder 
 * of the list 
 * 
 * g2 recursively calls g1
 * both functions use an accumulator for accumulating results.
 *)
let group l = 
    let rec g1 l1 acc =
        match (l1,acc) with
            ([], _)                 -> (l1,acc)
        | (a::t, [])                -> g1 t [a]
        | (a::t, b::_) when a = b   -> g1 t (a::acc)
        | _                         -> (l1,acc)
    in
        let rec g2 l2 acc =
            match g1 l2 [] with
                ([], group) -> group::acc
            | (tail, group) -> group::(g2 tail acc)
        in
            g2 l []
;;

(* 
 * Test 
 *)
let rec print_list l =
    match l with
       [] -> ()
   | h::t -> Printf.printf "%s" h; print_list t
;;

let rec print_lol l =
    match l with
        [] -> ()
    | h::t -> print_list h ; Printf.printf " "; print_lol t
;;

let test l = print_lol (pack l); Printf.printf "\n";;

test ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];; 

