(* Problem 02: Find the last but one element of a list *)


let penultimo0 l = (List.nth l ((List.length l) - 2))
(* discussion:
   raises Invalid_argument "List.nth" if list is shorter than two. *)

let rec penultimo1 = function
    [] | [_]     -> invalid_arg "penultimo1" (* see manual Chap. 20.2 Module Pervasives *)
  | e :: _ :: [] -> e                        (* pattern could as well be [e; _] *)
  | _ :: rest    -> penultimo1 rest
(* explained:
   an empty list or a one-element list lead to an exception.
   Note the underscore as anonymous placeholder.
   Then we check the "found" case, returning the value if found.
   Next we only advance one element in the list, and start over. *)

(** test **)
let () =
  Printf.printf "Test 0: %s\n" (penultimo0 ["a";"b";"c";"d"]);
  Printf.printf "Test 1: %s\n" (penultimo1 ["a";"b";"c";"d"]);

