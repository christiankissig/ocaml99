(* Count the leaves of a binary tree
    A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.

    % count_leaves(T,N) :- the binary tree T has N leaves 
*)

type bin_tree = 
		Leaf of string
	|	Node of string * bin_tree * bin_tree
;;

let rec count_leaves t =
        match t with
                Leaf(s) -> 1
        |       Node(s,tl,tr) -> 1 + (count_leaves tl) + (count_leaves tr)
;;
