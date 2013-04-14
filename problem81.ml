(* P81 (**) Path from one node to another one

Write a predicate path(G,A,B,P) to find an acyclic path P from node A to node
b in the graph G. The predicate should return all paths via backtracking.*)

let path g a b =
    let rec path a b p =
        (* avoid circles *)
        if( List.mem a p ) then [] else
        (* path found *)
            if a=b  then [p@[a]] else

        List.flatten
        ( List.map
        ( fun (x,y) -> if (x=a) then (path y b (p@[a])) else [] )
            g )
    in
    path a b []
;;

(* test *)
List.iter
(fun p -> (List.iter (Printf.printf "%d ") p; Printf.printf "\n"))
(path [(1,2);(2,3);(1,3);(3,4);(4,2);(5,6)] 1 4)
;;

Printf.printf "\n";;

List.iter
(fun p -> (List.iter (Printf.printf "%d ") p; Printf.printf "\n"))
(path [(1,2);(2,3);(1,3);(3,4);(4,2);(5,6)] 2 6 )
;;

