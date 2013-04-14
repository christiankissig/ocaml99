(* P97 (**) Sudoku

Sudoku puzzles go like this:

Problem statement                 Solution

  .  .  4 | 8  .  . | .  1  7      9  3  4 | 8  2  5 | 6  1  7
          |         |                      |         |
  6  7  . | 9  .  . | .  .  .      6  7  2 | 9  1  4 | 8  5  3
          |         |                      |         |
  5  .  8 | .  3  . | .  .  4      5  1  8 | 6  3  7 | 9  2  4
  --------+---------+--------      --------+---------+--------
  3  .  . | 7  4  . | 1  .  .      3  2  5 | 7  4  8 | 1  6  9
          |                                |
  .  6  9 | .  .  . | 7  8  .      4  6  9 | 1  5  3 | 7  8  2
          |         |                      |         |
  .  .  1 | .  6  9 | .  .  5      7  8  1 | 2  6  9 | 4  3  5
  --------+---------+--------      --------+---------+--------
  1  .  . | .  8  . | 3  .  6      1  9  7 | 5  8  2 | 3  4  6
          |         |                      |         |
  .  .  . | .  .  6 | .  9  1      8  5  3 | 4  7  6 | 2  9  1
          |         |                      |         |
  2  4  . | .  .  1 | 5  .  .      2  4  6 | 3  9  1 | 5  7  8


Every spot in the puzzle belongs to a (horizontal) row and a (vertical)
column, as well as to one single 3x3 square (which we call "square" for
short). At the beginning, some of the spots carry a single-digit number
between 1 and 9. The problem is to fill the missing spots with digits in such
a way that every number between 1 and 9 appears exactly once in each row, in
each column, and in each square.*)


(*
DataTypes & Notation :

	The board is implemented as a hash with int * int keys and int list values.
	A position in the board is a pair of int's.
	The field at a position is the list of possible values from [1-9].
	A position is assigned a value, if the field at the position is the singleton of that value.	
*)


(** General List Functions ***)

let remove_if t l = List.fold_right ( fun x l -> if ( t x ) then l else x::l ) l [] ;;
let remove_elt x l = remove_if ( fun y -> y = x ) l ;;
let remove_duplicates l = List.fold_right ( fun x l -> if ( List.mem x l ) then l else x::l ) l [];;
let list_diff l1 l2 = remove_if ( fun x -> List.mem x l2 ) l1 ;;
let rec head n l = if ( ( l = [] ) || ( n = 0 ) ) then [] else ( List.hd l ) :: ( head ( n - 1 ) ( List.tl l ) ) ;;

(** General Board Accessors ***)

let sample_board =
[	[ 0; 0; 4; 8; 0; 0; 0; 1; 7];
	[ 6; 7; 0; 9; 0; 0; 0; 0; 0];
	[ 5; 0; 8; 0; 3; 0; 0; 0; 4];
	[ 3; 0; 0; 7; 4; 0; 1; 0; 0];
	[ 0; 6; 9; 0; 0; 0; 7; 8; 0];
	[ 0; 0; 1; 0; 6; 9; 0; 0; 5];
	[ 1; 0; 0; 0; 8; 0; 3; 0; 6];
	[ 0; 0; 0; 0; 0; 6; 0; 9; 1];
	[ 2; 4; 0; 0; 0; 1; 5; 0; 0]	];;

(* a variation which gives multiple solutions *)
(*let sample_board =
[	[ 0; 0; 0; 8; 0; 0; 0; 1; 7];
	[ 0; 0; 0; 9; 0; 0; 0; 0; 0];
	[ 0; 0; 0; 0; 3; 0; 0; 0; 4];
	[ 3; 0; 0; 7; 4; 0; 1; 0; 0];
	[ 0; 6; 9; 0; 0; 0; 7; 8; 0];
	[ 0; 0; 1; 0; 6; 9; 0; 0; 5];
	[ 1; 0; 0; 0; 8; 0; 3; 0; 6];
	[ 0; 0; 0; 0; 0; 6; 0; 9; 1];
	[ 2; 4; 0; 0; 0; 1; 5; 0; 0]	];;*)



(* list all positions of the board *)
let get_all_positions = List.flatten ( List.map ( fun i -> List.map ( fun j -> (i,j) ) [0;1;2;3;4;5;6;7;8] ) [0;1;2;3;4;5;6;7;8] );;
(* list the positions in the same row *)
let get_row_positions (i,j) = List.map ( fun j2 -> (i,j2) ) [0;1;2;3;4;5;6;7;8];;
(* list the positions in the same column *)
let get_col_positions (i,j) = List.map ( fun i2 -> (i2,j) ) [0;1;2;3;4;5;6;7;8];;
(* list the positions in the same block *)
let get_block_positions (i,j) =
	let rec get_block_positions (i,j) i2 j2 =
		if ( j2 > ( ( j / 3 ) * 3 ) + 2 ) then get_block_positions (i,j) ( i2 + 1 ) ( ( j / 3 ) * 3) else
		if ( i2 > ( ( i / 3 ) * 3 ) + 2 ) then [] else
		(i2,j2) :: ( get_block_positions (i,j) i2 ( j2 + 1 ) )
	in
		get_block_positions (i,j) ( ( i / 3 ) * 3 ) ( ( j / 3 ) * 3 )
;;

(** Board I/O Functions ***)

(* reads a list of lists into the board hash *)
let read_board bl =
	let bh = Hashtbl.create 81 in
	let rec read_row rl i j =
		match rl with
			[]	-> ()
		|	h::t	-> 
				if ( h = 0 )
				then Hashtbl.add bh (i,j) [1;2;3;4;5;6;7;8;9]
				else Hashtbl.add bh (i,j) [h] ;
				read_row t i ( j + 1 )
	in
	let rec read_board bl i =
		match bl with
			[]	-> ()
		|	h::t	->
				read_row h i 0 ;
				read_board t ( i + 1 )
	in
		read_board bl 0 ;
		bh
;;

(* prints a board hash *)
let rec print_board bh =
	List.iter ( fun i ->
		List.iter ( fun j ->
			let poss = Hashtbl.find bh (i,j) in
			if ( List.length poss > 5 )
			then begin List.iter prerr_int ( head 3 poss ); prerr_string ".." end
			else List.iter prerr_int poss ;
			Printf.fprintf stderr "\t" )
		[0;1;2;3;4;5;6;7;8] ;
		Printf.fprintf stderr "\n" )
		[0;1;2;3;4;5;6;7;8] ;
	Printf.fprintf stderr "\n"
;;

(**  Update Functions ***)

(* given a list of fields to update, this function removes all possible values which are assigned to a position in the neighbourhood, i.e. the positions in the same row, column, or block. *)
let rec update_fields bh lpos =
	if ( lpos = [] ) then () else
	let pos = List.hd lpos in

	if ( List.length ( Hashtbl.find bh pos ) < 2 ) then ( update_fields bh ( List.tl lpos ) ) else
	
	let neighbours = ( get_row_positions pos ) @ ( get_col_positions pos ) @ ( get_block_positions pos ) in
	let assigned = List.flatten ( remove_if 
			( fun field -> ( ( List.length field ) > 1 ) )
			( List.map ( Hashtbl.find bh ) neighbours ) )
	in
	
	let old_poss = Hashtbl.find bh pos in
	let new_poss = list_diff old_poss assigned in
	
	Hashtbl.replace bh pos new_poss;
	
	if ( List.length old_poss ) = ( List.length new_poss )
	then update_fields bh ( List.tl lpos )
	else update_fields bh ( remove_duplicates ( ( List.tl lpos ) @ neighbours ) )
;;

(* update_board first calls update_fields to infer the valuation. it then tries all possible valuations in the least ambigious field ( a field of shortest length ) *)
let rec update_board board =
	(* infer the valuation *)
	update_fields board ( remove_if ( fun pos -> List.length ( Hashtbl.find board pos ) < 2 ) get_all_positions ) ;

	(* the following lists the ambigious positions AFTER the inference *)
	let ambigious = remove_if ( fun pos -> List.length ( Hashtbl.find board pos ) < 2 ) get_all_positions in 
	
	if ( List.exists ( fun field -> List.length ( Hashtbl.find board field ) < 1 ) ambigious )
	then () (* inconsistent layout *)
	else

	if ( List.length ambigious = 0 )
	then print_board board (* solved, then print *)
	else
		let ambigious = List.sort ( fun pos1 pos2 -> compare 
								( List.length ( Hashtbl.find board pos1 ) )
								( List.length ( Hashtbl.find board pos2 ) )
					) ambigious in
		( List.iter 	(* try all possible values for a least ambigious field *)
			( fun x -> 
				let new_board = board in
				Hashtbl.replace new_board ( List.hd ambigious ) [x];
				update_board new_board ;
			)
			( Hashtbl.find board ( List.hd ambigious ) ) )
;;

(** Main ***)

let board = read_board sample_board in 

	print_board board;

	update_board board;
	prerr_newline ();
;;