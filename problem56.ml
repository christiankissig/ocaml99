(*# P56 (**) Symmetric binary trees
# 
# Let us call a binary tree symmetric if you can draw a vertical line through the
# root node and then the right subtree is the mirror image of the left subtree.
# Write a predicate symmetric/1 to check whether a given binary tree is
# symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is
# the mirror image of another. We are only interested in the structure, not in
# the contents of the nodes.*)

type bin_tree = 
		Leave of string
	|	Node of string * bin_tree * bin_tree
;;

let symmetric t =
	let rec mirrored t1 t2 =
		match t1 with
			Leave a 	->
				begin
					match t2 with
						Leave b		-> a = b
					|	Node(a,t21,t22) -> false
				end
		|	Node(a,t11,t12)	->
				begin
					match t2 with
						Leave b		-> false
					|	Node(b,t21,t22)	->
							( a = b ) &&
							( mirrored t11 t22 ) &&
							( mirrored t12 t21 )
				end
	in
		match t with	
			Leave a		-> true
		|	Node(a,t1,t2)	-> mirrored t1 t2
;;

if ( symmetric ( Node ( "a", Node ( "c", Leave "b", Leave "a") , Node ( "c", Leave "a", Leave "b" ) ) ) )
then ( Printf.printf "ok\n" )
else ( Printf.printf "failed\n" );;