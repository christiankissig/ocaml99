(*# P71 Determine the internal path length of a tree
# 
# We define the internal path length of a multiway tree as the total sum of the
# path lengths from the root to all nodes of the tree. By this definition, the
# tree in the figure of problem P70 has an internal path length of 9. Write a
# predicate ipl(Tree,IPL) for the flow pattern (+,-). *)

type multi_tree =
        Leaf of string
    |   Node of string * multi_tree list
;;

let ipl t =
        let rec ipl t pl =
                match t with
                        Leaf a -> pl+1
                |       Node (a,l) ->
                                List.fold_left (+) 0 
                                (List.map (fun st -> ipl st (pl+1)) l)
        in 
                ipl t 0
;;
