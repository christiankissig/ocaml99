(*# P50 (***) Huffman code.
# 
# First of all, consult a good book on discrete mathematics or algorithms for a
# detailed description of Huffman codes!
# 
# We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. 
# Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. 
# 
# Our objective is to construct a list hc(S,C) terms, where C is the Huffman code
# word for the symbol S. In our example, the result could be Hs = [hc(a,'0'),
# hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')]
# [hc(a,'01'),...etc.]. The task shall be performed by the predicate huffman/2
# defined as follows:
# 
# % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
# 
# Binary Trees
# 
# A binary tree is either empty or it is composed of a root element and two
# successors, which are binary trees themselves.  In Lisp we represent the empty
# tree by 'nil' and the non-empty tree by the list (X L R), where X denotes the
# root node and L and R denote the left and right subtree, respectively. The
# example tree depicted opposite is therefore represented by the following list:
# 
# (a (b (d nil nil) (e nil nil)) (c nil (f (g nil nil) nil)))
# 
# Other examples are a binary tree that consists of a root node only:
# 
# (a nil nil) or an empty binary tree: nil.
# 
# You can check your predicates using these example trees. They are given as test
# cases in p54.lisp.*)

(* l is list of pairs [frequency,element] *)
let huffmann l =
    (* make list of freqs and lists of elem code pairs *)
    let l = List.map ( fun (f,e) -> (f,[(e,"")]) ) l in
    
    (* fun to sort by freq, ascending *)
    let sortbyfreq =
        List.sort ( fun (f,m) (g,n) -> if f>g then 1 else 0 )
    in

    (* fun to append character x to code c of each pair in list of elem code pairs *)
    let appendtocode x = List.map ( fun (e,c) -> (e,x^c) ) in

    let rec combine_rare l =
            (* sort by freq, a 1-step bubblesort would be better here *)
            let l = sortbyfreq l in
            match l with
            (* take two least frequent elements from list *)
            (f,m)::(g,n)::t ->
                combine_rare ((f+g,(appendtocode "0" m)@(appendtocode "1" n))::t)
            (* if list contains less than two elements *)
            | _ -> l
    in
        (* return list of elem and code pairs *)
        match ( List.hd ( combine_rare l ) ) with ( f, m ) -> m
;;

let print_codes l =
    List.iter ( fun (e,c) -> Printf.printf "%s=%s " e c ) l;
    Printf.printf "\n"
;;

print_codes ( huffmann [(45,"a");(13,"b");(12,"c");(16,"d");(9,"e");(5,"f")] );;
