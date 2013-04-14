(* A string representation of binary trees

    Somebody represents binary trees as strings of the following type (see example opposite):

    a(b(d,e),c(,f(g,)))

    a) Write a Prolog predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term). Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, combine the two predicates in a single predicate tree_string/2 which can be used in both directions.

    b) Write the same predicate tree_string/2 using difference lists and a single predicate tree_dlist/2 which does the conversion between a tree and a difference list in both directions.

    For simplicity, suppose the information in the nodes is a single letter and there are no spaces in the string. 
*)

type bin_tree = 
		Leaf of string
	|	Node of string * bin_tree * bin_tree
;;

let rec tree_to_string t =
        match t with
                Leaf s -> s
        |       Node (s,tl,tr) -> 
                        String.concat "" 
                                [s;"(";tree_to_string tl;",";tree_to_string tr;")"]
;;
