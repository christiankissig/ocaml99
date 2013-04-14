(*# P92 (***) Von Koch's conjecture
# 
# Several years ago I met a mathematician who was intrigued by a problem for
# which he didn't know a solution. His name was Von Koch, and I don't know
# whether the problem has been solved since.
# 
# Anyway the puzzle goes like this: Given a tree with N nodes (and hence N-1
# edges). Find a way to enumerate the nodes from 1 to N and, accordingly, the
# edges from 1 to N-1 in such a way, that for each edge K the difference of its
# node numbers equals to K. The conjecture is that this is always possible.
# 
# For small trees the problem is easy to solve by hand. However, for larger
# trees, and 14 is already very large, it is extremely difficult to find a
# solution. And remember, we don't know for sure whether there is always a
# solution!
# 
# Write a predicate that calculates a numbering scheme for a given tree. What is
# the solution for the larger tree pictured above?*)


(* extract the nodes of a tree *)
let get_nodes = 
    List.fold_left
    ( fun l (x,y) ->
        (if (List.mem x l) then [] else [x])@
        (if (List.mem y l) then l else (y::l)) )
    []
;;

(* generate list of 1..n elements *)
let gen_list n =
    let rec gen_list n l =
        if(n<1) then l else
        gen_list (n-1) (n::l)
    in
    gen_list n []
;;

(* remove all occurrences of x from a list *)
let rem_mem x =
    List.fold_left
    ( fun l y -> if (x=y) then l else (y::l))
    []
;;

(* List.iter with remainder *)
let iter_rmd f =
    let rec iter_rmd f p l =
        match l with
        [] -> ()
        | h::t -> (f p h t; iter_rmd f (p@[h]) t)
    in
    iter_rmd f []
;;

(* print all admissible enumerations of nodes in tree *)
let vankoch tree =
    let nodes = get_nodes tree in
    let edges = gen_list ((List.length nodes)-1) in
    (* tree, current node, current number, visited edges, nodes and edges left *)
    let rec vankoch t v n e enum = 

        if(t=[]) then 
            (
                List.iter (fun (i,n)->Printf.printf "(%d,%d) " i n) enum;
                Printf.printf "\n";
                flush stdout
            )
        else

        (* next edge adjacent to a visited one *)
        let (x,y) = List.find (fun (x,y)->(List.mem x v)||(List.mem y v)) t in
        (* remaining edges in tree *)
        let rem_edges = List.fold_left 
            (fun t (u,v) ->if (x=u)&&(y=v) then t else ((u,v)::t))
            []
            t 
        in  
        (* fix orientation *)
        let (x,y) = if (List.mem x v) then (x,y) else (y,x) in
        (* get number of x from enum *)
        let i = List.fold_left (fun i (z,h) -> if (z=x) then h else i) 0 enum in

        iter_rmd 
        (fun p h t ->
            (* prospective edge number *)
            let d = abs (i-h) in
            (* d available? *)
            if(List.mem d e)
            then 
                vankoch rem_edges (y::v) (p@t) (rem_mem d e) ((y,h)::enum)
            else ())
        n
    in
    if(tree=[]) then () else
    (iter_rmd
    (fun p h t ->
        vankoch tree [List.hd nodes] (p@t) edges [(List.hd nodes,h)])
    nodes)
;;

(* test *)
vankoch [(1,6);(2,6);(3,6);(4,6);(5,6);(5,7);(5,8);(8,9);(5,10);(10,11);(11,12);(11,13);(13,14)]
;;
