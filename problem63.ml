(*# P63 (**) Construct a complete binary tree
# 
# A complete binary tree with height H is defined as follows: The levels
# 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i,
# note that we start counting the levels from 1 at the root). In level H, which
# may contain less than the maximum possible number of nodes, all the nodes are
# "left-adjusted". This means that in a levelorder tree traversal all internal
# nodes come first, the leaves come second, and empty successors (the nil's which
# are not really nodes!) come last.
# 
# Particularly, complete binary trees are used as data structures (or addressing
# schemes) for heaps.
# 
# We can assign an address number to each node in a complete binary tree by
# enumerating the nodes in levelorder, starting at the root with number 1. In
# doing so, we realize that for every node X with address A the following
# property holds: The address of X's left and right successors are 2*A and 2*A+1,
# respectively, supposed the successors do exist. This fact can be used to
# elegantly construct a complete binary tree structure. Write a predicate
# complete-binary-tree/2 with the following specification:
# 
# % complete-binary-tree(N,T) :- T is a complete binary tree with N nodes. (+,?)
# 
# Test your predicate in an appropriate way.*)

type bin_tree =
                Leaf of string
        |       Node of string * bin_tree * bin_tree
;;

let rec pow2 x =
        if x < 1
        then 1
        else 2 * ( pow2 ( x-1 ) )
;;

let complete_binary_tree n = 
        let rec complete_binary_tree x d =

                (* number of left successor node *)
                let xl = x + (pow2 d) + (x - (pow2 d)) in

                if xl > n
                then 
                        Leaf(string_of_int x)
                else

                if xl+1 > n
                then 
                        Node (  string_of_int x, 
                                complete_binary_tree xl (d+1) , 
                                Leaf(string_of_int (xl+1)) )
                else

                        Node (  string_of_int x, 
                                complete_binary_tree xl (d+1), 
                                complete_binary_tree (xl+1) (d+1) )
        in
                complete_binary_tree 1 0
;;

let rec string_of_binary_tree t =
        match t with
                Leaf s -> s
                | Node(s,l,r) -> 
                                Printf.sprintf 
                                        "(%s,%s,%s)" s
                                        (string_of_binary_tree l) 
                                        (string_of_binary_tree r)
;;

let print_binary_tree t = Printf.printf "%s" (string_of_binary_tree t);;

print_binary_tree (complete_binary_tree 43);;
