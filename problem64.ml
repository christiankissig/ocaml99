(*# P64 (**) Layout a binary tree (1)
# 
# Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a
# preparation for drawing the tree, a layout algorithm is required to determine
# the position of each node in a rectangular grid. Several layout methods are
# conceivable, one of them is shown in the illustration below.
# 
# In this layout strategy, the position of a node v is obtained by the following
# two rules:
# 
# * x(v) is equal to the position of the node v in the inorder sequence
# * y(v) is equal to the depth of the node v in the tree
# 
# 
# 
# In order to store the position of the nodes, we extend the Prolog term
# representing a node (and its successors) as follows:
# 
# % nil represents the empty tree (as usual)
# % t(W,X,Y,L,R) represents a (non-empty) binary tree with root W "positioned" at (X,Y), and subtrees L and R
# 
# Write a predicate layout-binary-tree/2 with the following specification:
# 
# % layout-binary-tree(T,PT) :- PT is the "positioned" binary tree obtained from
# the binary tree T. (+,?)
# 
# Test your predicate in an appropriate way.*)

type ext_bin_tree =
        Leaf of string
        | Node of string * ext_bin_tree * ext_bin_tree
;;

type ext_bin_tree =
        ExtLeaf of string * int * int
        | ExtNode of string * ext_bin_tree * ext_bin_tree * int * int
;;

let rec nodes t =
        match t with
        Leaf(a) -> 1
        | Node(a,l,r) -> 1+(nodes l)+(nodes r)
;;

let rec height t =
        match t with
        Leaf(a) -> 1
        | Node(a,l,r) -> 
                        let hl = height l
                        and hr = height r in
                        if hl<hr then (1+hr) else (1+hr)
;;

let ext_tree t =
        let rec ext_tree t x y =
                match t with
                Leaf(a) -> ExtLeaf(a,x+1,y)
                | Node(a,l,r) -> 
                                let ln = nodes l in
                                ExtNode( a,
                                        ext_tree l x (y-1),
                                        ext_tree r (x+ln+1) (y-1),
                                        x+ln+1, y)
        in
        ext_tree t 0 ((height t)+1)
;;

let rec string_of_ext_tree t = 
        match t with
        ExtLeaf(s,x,y) -> Printf.sprintf "(%s,%d,%d)" s x y
        | ExtNode(s,l,r,x,y) -> Printf.sprintf "(%s,%s,%s,%d,%d)" s
                (string_of_ext_tree l)
                (string_of_ext_tree r)
                x y
;;

Printf.printf "%s\n" (string_of_ext_tree (ext_tree
(Node("a",Node("b",Leaf("c"),Leaf("d")),Leaf("e")))));;
