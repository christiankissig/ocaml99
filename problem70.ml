(*# P70 (**) Tree construction from a node string
# 
# We suppose that the nodes of a multiway tree contain single characters. In the
# depth-first order sequence of its nodes, a special character ^ has been
# inserted whenever, during the tree traversal, the move is a backtrack to the
# previous level.
# 
# By this rule, the tree in the figure opposite is represented as: afg^^c^bd^e^^^
# 
# Define the syntax of the string and write a predicate tree(String,Tree) to
# construct the Tree when the String is given. Work with atoms (instead of
# strings). Make your predicate work in both directions.*)
