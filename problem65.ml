(*# P65 (**) Layout a binary tree (2)
# 
# An alternative layout method is depicted in the illustration opposite. Find out
# the rules and write the corresponding Prolog predicate. Hint: On a given level,
# the horizontal distance between neighboring nodes is constant.
# 
# Use the same conventions as in problem P64 and test your predicate in an
# appropriate way.*)

type ext_bin_tree =
        Leaf of string
        | Node of string * ext_bin_tree * ext_bin_tree
;;

type ext_bin_tree =
        ExtLeaf of string * int * int
        | ExtNode of string * ext_bin_tree * ext_bin_tree * int * int
;;

let rec height t =
        match t with
        Leaf(s) -> 1
        | Node(s,l,r) -> 
                        let hl = height l 
                        and hr = height r in
                        if hl < hr then (1+hr) else (1+hl)
;;

let rec pow2 e = if e < 1 then 1 else 2*(pow2 (e-1));;

let sum = List.fold_left (+) 0;;

let rec range x = if x<0 then [] else ((range(x-1))@[x])

let ext_tree t =
        let h = height t in

        let rec ext_tree t x y w =
                match t with
                | Leaf(s) ->  ExtLeaf(s,x,y)
                | Node(s,l,r) -> 
                                ExtNode(s,
                                        ext_tree l (x-w) (y-1) (w/2),
                                        ext_tree r (x+w) (y-1) (w/2),
                                        x,y)
        in
        
               ext_tree t ((sum (List.map pow2 (range (h-2))))+1) h (pow2 (h-2))
;;

let rec string_of_ext_tree t = 
        match t with
        ExtLeaf(s,x,y) -> Printf.sprintf "(%s,%d,%d)" s x y
        | ExtNode(s,l,r,x,y) -> Printf.sprintf "(%s,%s,%s,%d,%d)" s
                (string_of_ext_tree l)
                (string_of_ext_tree r)
                x y
;;

