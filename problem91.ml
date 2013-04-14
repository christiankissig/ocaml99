(* Knight's tour
   Another famous problem is this one: How can a knight jump on
   an NxN chessboard in such a way that it visits every square exactly
   once?

   Hints: Represent the squares by pairs of their coordinates of
   the form X/Y, where both X and Y are integers between 1 and N.
   (Note that '/' is just a convenient functor, not division!)
   Define the relation jump(N,X/Y,U/V) to express the fact that a
   knight can jump from X/Y to U/V on a NxN chessboard. And finally,
   represent the solution of our problem as a list of N*N knight
   positions (the knight's tour). *)

let knights =
  let rec knights t =
    if ( List.length t = 64 ) then [t] else
    let neighbours x y =
      List.filter
	( fun (x,y) -> ( 0<=x && x<=7 && 0<=y && y<=7 ) )
	[(x+1,y+2);(x+1,y-2);(x+2,y+1);(x+2,y-1);(x-1,y-2);(x-1,y+2);(x-2,y-1);(x-2,y+1)]
    in
      match t with
      [] -> []
      | (x,y)::r ->
	List.concat
	( List.map
	  ( fun (xn,yn) ->
	      if ( List.exists ( fun (x,y) -> x=xn && y=yn ) t )
	      then []
	      else knights ((xn,yn)::t) )
	  ( neighbours x y ) )
  in
    List.concat
    ( List.concat
    ( List.map
    ( fun x ->
      List.map
      ( fun y ->
	knights [(x,y)] )
      [0;1;2;3;4;5;6;7] )
    [0;1;2;3;4;5;6;7] ) )
;;

let rec print_lop l =
    match l with
    [] -> ()
    | (x,y)::t -> Printf.printf "(%d,%d) " x y; print_lop t
  in
let rec print_lol l =
    match l with
    [] -> ()
    | h::t -> print_lop h; Printf.printf "\n"; print_lol t
  in
    print_lol ( knights )
;;