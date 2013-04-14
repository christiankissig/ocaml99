(*# P69 (**) Dotstring representation of binary trees
# 
# We consider again binary trees with nodes that are identified by single
# lower-case letters, as in the example of problem P67. Such a tree can be
# represented by the preorder sequence of its nodes in which dots (.) are
# inserted where an empty subtree (nil) is encountered during the tree
# traversal. For example, the tree 
#
# Node("a",Node("b",Leaf("d"),Leaf("e")),Node("c",Leaf("f"),Leaf("g"))))
#
# is represented as
#
# "abd..e..cf..g..."
#
# First, try to establish a syntax (BNF or syntax diagrams)
# and then write a predicate tree-dotstring/2 which does the conversion in both
# directions. Use difference lists.
*)

type bin_tree = Leaf of string | Node of string * bin_tree * bin_tree ;;

let rec dotstring t =
        match t with
        Leaf(a) -> Printf.sprintf "%s.." a
        | Node(a,l,r) -> Printf.sprintf "%s%s%s" a (dotstring l) (dotstring r)
;;

Printf.printf "%s\n" 
        (dotstring
                (Node("a",Node("b",Leaf("d"),Leaf("e")),Node("c",Leaf("f"),Leaf("g")))))
;;
