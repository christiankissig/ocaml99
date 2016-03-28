(* Problem 01: Find the last element of a list *)

exception Empty_list
(* discussion:
   we could use one of the predefinded exceptions,
   see OCaml manual Chap. 20.1, but defining an exception is no problem. *)


let last0 l = List.nth l ((List.length l) - 1)
(* explained:
   Count the list length and pick the last by it's position.
   List.length runs through the list, and nth as well. *)

let last1 l = List.hd (List.rev l)
(* explained:
   Reverse the list and take the first.
   Builds a new reversed list, while we just want one element. *)

let rec last2 l =
    match l with
        []  -> raise Empty_list
    |   h::[]   -> h
    |   h::t    -> last2 t
(* explained:
   If you have a one element list, return the element,
   else remove the first element and repeat (in fact: recurse). 
   This can be written more idiomatic,
   avoiding any unnecessary parameter bindings. *)

let rec last3 = function
  | []      -> raise Empty_list
  | h :: [] -> h
  | _ :: t  -> last3 t
(* explained:
   Logic is the same as last_2, just some minor optimizations.
   This should be the best version, because you go just once
   through the list without keeping unwanted data. *)

let () =
  let testlist = ["I";"am";"the";"very"; "last!"] in
    Printf.printf "version 0: %s\n" ( last0 testlist);
    Printf.printf "version 1: %s\n" ( last1 testlist);
    Printf.printf "version 2: %s\n" ( last2 testlist);
    Printf.printf "version 3: %s\n" ( last3 testlist)

(*
   run with: ocaml problem01.ml
   or paste a function into the toplevel and terminate with ;;
*)