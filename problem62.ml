(* Collect the internal nodes of a binary tree in a list
    An internal node of a binary tree has either one or two non-empty successors. Write a predicate internals/2 to collect them in a list.

    % internals(T,S) :- S is the list of internal nodes of the binary tree T. *)

type bin_tree = 
		Leaf of string
	|	Node of string * bin_tree * bin_tree
;;

let rec collect_nodes t =
        match t with
                Leaf(s) -> []
        |       Node(s,tl,tr) -> List.flatten [[s];collect_nodes tl;collect_nodes tr]
;;
