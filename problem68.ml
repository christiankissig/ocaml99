(* Preorder and inorder sequences of binary trees
    We consider binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67.

    a) Write predicates preorder/2 and inorder/2 that construct the preorder and inorder sequence of a given binary tree, respectively. The results should be atoms, e.g. 'abdecfg' for the preorder sequence of the example in problem P67.

    b) Can you use preorder/2 from problem part a) in the reverse direction; i.e. given a preorder sequence, construct a corresponding tree? If not, make the necessary arrangements.

    c) If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given, then the tree is determined unambiguously. Write a predicate pre_in_tree/3 that does the job.

    d) Solve problems a) to c) using difference lists. Cool! Use the predefined predicate time/1 to compare the solutions.

    What happens if the same character appears in more than one node. Try for instance pre_in_tree(aba,baa,T).
*)

type bin_tree = 
		Leaf of string
	|	Node of string * bin_tree * bin_tree
;;

let rec preorder t =
        match t with
                Leaf s -> [s]
        |       Node (s,tl,tr) -> List.flatten [[s];preorder tl;preorder tr]
;;

let rec inorder t =
        match t with
                Leaf s -> [s]
        |       Node (s,tl,tr) -> List.flatten [inorder tl;[s];inorder tr]
;;
