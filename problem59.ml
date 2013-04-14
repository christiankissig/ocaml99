(*# P59 (**) Construct height-balanced binary trees
# 
# In a height-balanced binary tree, the following property holds for every node:
# The height of its left subtree and the height of its right subtree are almost
# equal, which means their difference is not greater than one.
# 
# Write a predicate hbal-tree/2 to construct height-balanced binary trees for a
# given height. The predicate should generate all solutions via backtracking. Put
# the letter 'x' as information into all nodes of the tree.
# 
# Example:
# * hbal-tree(3,T).
# T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
# T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
# etc......No*)

type bin_tree =
	Leave of string
|	Node of string * bin_tree * bin_tree
;;

let rec hbal_tree n =
	if ( n < 2 )
		(* note: if there is no tree of n nodes, then the algorithm produces *)
		(* a tree with a number least above the given n *)
	then ( Leave "x" )
	else
		let t1 = hbal_tree ( ( n - 1 ) / 2 )
		and t2 = hbal_tree ( n - 1 - ( ( n -1 ) / 2 ) )
		in
			Node ( "x", t1, t2 )
;;

if( hbal_tree 3 = Node( "x", Leave "x", Leave "x" ) )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;