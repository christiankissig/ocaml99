(*# P73 (**) Lisp-like tree representation
# 
# There is a particular notation for multiway trees in Lisp. Lisp is a prominent
# functional programming language, which is used primarily for artificial
# intelligence problems. As such it is one of the main competitors of Prolog. In
# Lisp almost everything is a list, just as in Prolog everything is a term.
# 
# The following pictures show how multiway tree structures are represented in Lisp.
# 
# Note that in the "lispy" notation a node with successors (children) in the
# tree is always the first element in a list, followed by its children. The
# "lispy" representation of a multiway tree is a sequence of atoms and
# parentheses '(' and ')', which we shall collectively call "tokens". We can
# represent this sequence of tokens as a Prolog list; e.g. the lispy expression
# (a (b c)) could be represented as the Prolog list ['(', a, '(', b, c, ')',
# ')']. Write a predicate tree-ltl(T,LTL) which constructs the "lispy token
# list" LTL if the tree is given as term T in the usual Prolog notation.
# 
# Example:
# * tree-ltl(t(a,[t(b,[]),t(c,[])]),LTL).
# LTL = ['(', a, '(', b, c, ')', ')']
# 
# As a second, even more interesting exercise try to rewrite tree-ltl/2 in a way
# that the inverse conversion is also possible: Given the list LTL, construct
# the Prolog tree T. Use difference lists.
*)

type multi_tree =
        Leaf of string
    |   Node of string * multi_tree list
;;

let rec tree_to_ltl t =
        match t with
                Leaf a -> [a]
        |       Node (a,l) -> 
                        List.flatten[
                                [a,'('],
                                (List.flatten (List.map tree_to_ltl l)),
                                [')']
                                ]
;;
