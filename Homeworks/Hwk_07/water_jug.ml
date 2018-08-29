(* I use a type state, which is an int * int. It was pivitol in the
creating functions to find solutions, as it maintained what the 
current state was and the previous states were stored as well.

type state = int * int

The way I went about this problem was by creating a few
helper functions. Those functions and descriptions are
as follows:

   1. final_state: checkes the state to see if the four gallon
   bucket has 2 gallons in it. This is the stop condition in
   the search

   2. ok_state: checks to see if the current state has been 
   already visited. This prevents looping.

   3. ok_op: takes the current state and the state list with
   all previous states as input. From the current state, all
   of the possible jug operations list is filtered to only
   ones that are compatible for that state. When checking for
   compatible operations, the next possible state is examined 
   and if the ending state of the operation leads to a state
   inside of the previous states list, then that operation is 
   removed. This utilizes ok_state function

   4. update_state: Takes in the current state and an operation
   and performs that operation on the state. The new state is 
   returned

   5. create_opstring: Takes as input a (state * operation) list
   and returns a (operation * string) list. The main takeaway is 
   that the operations are unchanged, but the states that the 
   operation was conducted for is converted into a string with
   the describe method.

   The final method tying everything together was helper. The first 
   thing done was to check for an end condition i.e. if the four 
   gallon had 2 gallons inside. If not, a list of compatible 
   operations was generated for the state. If there were no compatible 
   operations, then an empty list was returned by the helper. 
   Otherwise the state is updated, the previos state was added
   to the previous states list, and the updated state and 
   corresponding operation is added to the (state * operation) list.
   helper is then called recursively.

   To finish off play, the helper function is called and match to 
   either an empty list(indicating no answer found) which returns 
   None, or a non empty list is returned which returns Some 
   (operation * string) list using the helper function create_opstring

*)


(*Code below was included with homework and
  not written by Connor Hanlon*)
let describe (four:int) (three:int) : string = 
  let describe' jug amount =
    "The " ^ string_of_int jug ^ " gallon jug " ^
      match amount with
      | 0 -> " is empty"
      | 1 -> " contains 1 gallon"
      | x -> " contains " ^ string_of_int x ^ " gallons"
  in
  describe' 4 four ^ ", " ^ describe' 3 three ^ "."

type operation = Fill4GallonJugFromTap
               | Fill3GallonJugFromTap
               | Empty4GallonJugOnGround
               | Empty3GallonJugOnGround
               | Fill4GallonJugFrom3GallonJug
               | Fill3GallonJugFrom4GallonJug
               | Empty4GallonJugInto3GallonJug
               | Empty3GallonJugInto4GallonJug


(* Code below is by Connor Hanlon*)
type state = int * int
type sop_list = (state * operation) list
type opstring = (operation * string) list
  
  
(*Checks to see if the state is the desired, and
if so returns true. Used to end or continue
searching in play*)
let final_state (s:state): bool =
  match s with
  |four, three -> (four=2) 


(*Checks to see if the state has been previously visited
and resides in the state list*)
let ok_state (s:state)(st:state list):bool=
  not (List.mem s st) 

(*Determines which operations are compatible with a state, which are
returned as a list of operations
*)
let ok_op (s:state)(st:state list): operation list =
  let oplist =  [Fill4GallonJugFromTap; Fill3GallonJugFromTap;
	      Empty4GallonJugOnGround; Empty3GallonJugOnGround;
	      Fill4GallonJugFrom3GallonJug; Fill3GallonJugFrom4GallonJug;
	      Empty4GallonJugInto3GallonJug; Empty3GallonJugInto4GallonJug]
  in
  let (x,y) = s in
  let my_filt (o:operation): bool=
    match o with
    | Fill4GallonJugFromTap -> x<4 && (ok_state (4,y) st)
    | Fill3GallonJugFromTap -> y<3 && (ok_state (x, 3) st)
    | Empty4GallonJugOnGround -> x>0 && (ok_state (0,y) st)
    | Empty3GallonJugOnGround -> y>0 && (ok_state (x,0) st)
    | Fill4GallonJugFrom3GallonJug ->
       (y>0) && (x+y>=4) && (ok_state (4, (4-y)) st)
    | Fill3GallonJugFrom4GallonJug ->
       (x>0) && (x+y>=3) && (ok_state ((x-3), 3) st)
    | Empty4GallonJugInto3GallonJug ->
       (x>0) && (x+y<=3) && (ok_state (0, (x+y)) st)
    | Empty3GallonJugInto4GallonJug ->
       (y>0) && (x+y<=4) && (ok_state ((x+y), 0) st)
  in
  List.filter my_filt oplist

(*updates a state with an operation*)
let update_state (s:state)(o:operation):state =
  match o, s with
  |Fill4GallonJugFromTap, (four, three) -> (4, three)
  |Fill3GallonJugFromTap, (four, three) -> (four, 3)
  |Empty4GallonJugOnGround, (four, three) ->(0, three)
  |Empty3GallonJugOnGround, (four, three) ->(four, 0)
  |Fill4GallonJugFrom3GallonJug, (four, three) ->(4, three -(4-four))
  |Fill3GallonJugFrom4GallonJug, (four, three) ->(four-(3-three), 3)
  |Empty4GallonJugInto3GallonJug, (four, three)->(0, four+three)
  |Empty3GallonJugInto4GallonJug, (four, three)->(four+three, 0)

(*Converts a sop_list to a opstring by iterating over the sop_list,
 using describe to
changed the state to a string and concat to opstring.
*) 
let rec create_opstring (sop: sop_list) (ops: opstring): opstring=
  match sop with
  | [] -> ops
  | head::tail ->
     match head with
     | (s1, s2), op -> let new_ops = (op, (describe s1 s2))::ops in
		       create_opstring tail new_ops

(*starting at a state of 0 gallons in four gallon jug and 0 gallons
in the three gallon jug, a list of operations/moves along with how much
water resided in each jug following an operation is created showing all
steps taken to reach a final state with 2 gallons of water in
the four gallon jug. See notes at top of this file for more in depth
breakdown.*)			 
let play ()=
  let rec helper (s:state)(st:state list)(sop: sop_list) : sop_list=
    if final_state s then sop
    else
      let ok_ops = ok_op s st in   
      match ok_ops with
      |[] -> []
      |hd::tail -> let updated_state = update_state s hd in
	 helper updated_state (s::st) ((updated_state,hd)::sop)
  in
  let ans_sop = helper (0,0) [] [] in
  match ans_sop with
  | [] -> None
  | ans -> Some (create_opstring ans [])



    
(*
let ok_op_random (s:state)(st:state list)(ops:operation list):operation list =
  let (x,y) = s in
  let my_filt (o:operation): bool=
    match o with
    | Fill4GallonJugFromTap -> x<4 
    | Fill3GallonJugFromTap -> y<3 
    | Empty4GallonJugOnGround -> x>0 
    | Empty3GallonJugOnGround -> y>0 
    | Fill4GallonJugFrom3GallonJug -> (y>0) && (x+y>=4) && (y<>4)
    | Fill3GallonJugFrom4GallonJug -> (x>0) && (x+y>=3) && (x<>3)
    | Empty4GallonJugInto3GallonJug -> (x>0) && (x+y<=3) 
    | Empty3GallonJugInto4GallonJug -> (y>0) && (x+y<=4) 
  in
  List.filter my_filt ops

    
let random_op (ops:operation list):operation =
  let sz = List.length ops in
  let rf = Random.int sz in
  List.nth ops rf

  
let play_random ()=
  let rec helper (s:state)(st:state list)(sop: sop_list) : sop_list=
    if final_state s then sop
    else
      let ok_ops = ok_op_random s st oplist in
      let choosen_op = random_op ok_ops in
      let ups = (update_state s choosen_op)
      let ans_sop = helper ups (s::st) ((ups,choosen_op)::sop) 
 *)
