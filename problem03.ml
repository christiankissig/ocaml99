(* 
    Problem 03: Find the k-th Element of a List
*)

let kthelement0 l k = List.nth l (k - 1)
(* explained:
   now this was easy. List.nth thows exceptions, see the manual. *)


let rec kthelement1 l k = match l with
      []        -> invalid_arg "kthelement: list exhausted"
    | e :: rest -> if k = 1 then e else kthelement1 rest (k - 1)
(* explained:
   we go through the list, dropping the first and decrementing k
   until we reach the desired position or the end of the list. *)

(*
 * This solution is due to Bart van Deenen. Thanks!
 *)
let rec kthelement2 l k = match (l,k) with
      ([],        _)   -> invalid_arg "kthelement: list empty"
    | (x :: _,    1)   -> x
    | (_ :: rest, _)   -> kthelement2 rest (k-1)
(* explained:
   This shows how you can match of more than one parameter,
   by turning it into a pair or tuple. *)

(*
 * Test
 *)
let () =
  let l = ["a";"b";"c";"d"] in
    Printf.printf "Version 0: %s\n" (kthelement0 l 3);
    Printf.printf "Version 1: %s\n" (kthelement1 l 3);
    Printf.printf "Version 2: %s\n" (kthelement2 l 3)
