(*# P27 (**) Group the elements of a set into disjoint subsets.
# 
# a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2,
# 3 and 4 persons? Write a function that generates all the possibilities and
# returns them in a list.
# 
# Example:
# * (group3 '(aldo beat carla david evi flip gary hugo ida))
# ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
# ... )
# 
# b) Generalize the above predicate in a way that we can specify a list of group
# sizes and the predicate will return a list of groups.
# 
# Example:
# * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
# ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
# ... )
# 
# Note that we do not want permutations of the group members; i.e. ((ALDO BEAT)
# ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference
# between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
# 
# You may find more about this combinatorial problem in a good book on discrete
# mathematics under the term "multinomial coefficients".*)


let group3 l =
    let rec group3 l n xs x r =
        if(n<1) then [] else
        if(n=1) then [(l@r)::xs] else

        match l with
        [] -> []
        | h::t -> 
            (group3 t n xs (h::x) r)@
            (group3 t n xs x (h::r))@
            (if ((List.length x)=0)
            then [] (* avoid empty parts *)
            else (group3 (r@t) (n-1) (x::xs) [h] []))
    in
    group3 l 3 [] [] []
;;

let group l ns =
    let rec group l n ns x xs r =
        if(n=0)
        then
            (match ns with
            [] -> if (List.length (l@r) = 0) then [x::xs] else []
            | nsh::nst -> (group (r@l) nsh nst [] (x::xs) []) )
        else

        match l with
        [] -> []
        | h::t ->
                ((group t n ns x xs (h::r))@
                (group t (n-1) ns (h::x) xs r))
    in
        if ((List.fold_left (+) 0 ns)=(List.length l))
        then
            (match ns with
            [] -> []
            | nsh::nst -> group l nsh nst [] [] [])
        else 
            []
;;
