(* Construct height-balanced binary trees with a given number of nodes
 
Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?
Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more difficult. Try to find a recursive statement and turn it into a predicate minNodes/2 defined as follwos:

% minNodes(H,N) :- N is the minimum number of nodes in a height-balanced binary tree of height H.
(integer,integer), (+,?)

On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have?

% maxHeight(N,H) :- H is the maximum height of a height-balanced binary tree with N nodes
(integer,integer), (+,?)

Now, we can attack the main problem: construct all the height-balanced binary trees with a given nuber of nodes.

% hbal_tree_nodes(N,T) :- T is a height-balanced binary tree with N nodes.

Find out how many height-balanced trees exist for N = 15.
*)

let rec min_nodes h =
    if h<1 then 0 else
    if h=1 then 1 else
    if h=2 then 3 else
    1 + ( min_nodes (h-1) ) + ( min_nodes ( h-2 ) )
;;

Printf.printf "%d\n" ( min_nodes 5 );;
