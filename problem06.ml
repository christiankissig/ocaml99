(* 
 * Find out whether a list is a palindrome. A palindrome 
 * can be read forward or backward; e.g. (x a m a x). 
 *)

let rec compare_lists l1 l2 =
    match (l1,l2) with
        ([], [])         -> true
    | (h1::t1, h2::t2)   ->
        if( h1 = h2 ) 
        then ( compare_lists t1 t2 )
        else false
    | (l1, l2) -> false
;;

let is_palindrom l = compare_lists l ( List.rev l );;

(*
 * This solution is due to Bart van Deenen. Thanks!
 *)
let is_palindrome_2 l =
    let rec ip l1 l2 =
        match (l1,l2) with
            ([],[])                     -> true
        | (h1::t1, h2::t2) when h1 = h2 -> ip t1 t2
        |  _                            -> false 
    in
        ip l (List.rev l)
;;

(* 
 * Test 
 *)
let test l = Printf.printf 
    "abcba is a palindrom : %b\n" 
    (is_palindrom l);;

test ["a";"b";"c";"b";"a"];;

