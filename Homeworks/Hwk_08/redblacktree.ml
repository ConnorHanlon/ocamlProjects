open Ordered
  
module type RedBlackSetSig = sig
  type elem
  type color = R | B
  type t = E | T of color * t * elem * t 

  val empty: t
  val insert: elem -> t -> t
  val member: elem -> t -> bool
  val isRedBlackTree: t -> bool

end

module RedBlackTree (O:OrderedSig):(RedBlackSetSig with type elem=O.t)=struct
  type elem = O.t
  type color = R | B
  type t = E | T of color * t * elem * t

  let empty = E
  let balance (col:color)(left:t)(element:elem)(right:t):t =
    match col, left, element, right with
    | B, T (R, T (R,a,x,b),y,c), z, d ->
       T (R, T (B,a,x,b),y ,T (B, c,z,d))
    | B, T (R,a,x,T (R,b,y,c)),z,d ->
       T (R, T (B,a,x,b),y ,T (B, c,z,d))
    | B, a, x, T (R, T (R,b,y,c),z,d) ->
       T (R, T (B,a,x,b),y ,T (B, c,z,d))
    | B, a, x, T (R,b,y,T (R,c,z,d)) ->
       T (R, T (B,a,x,b),y ,T (B, c,z,d))
    | _ -> T (col, left, element, right)
    
  let insert (element:elem)(set:t): t=
    let rec ins (someset:t) =
      match someset with
      | E -> T (R, E, element, E)
      | T (col, left, el, right) ->
	 if O.lt element el then balance col (ins left) el right
	 else if O.lt el element then balance col left el (ins right)
	 else someset
    in
     match ins set with
      | T(_,l,elemental,r) -> T(B,l,elemental,r)
      | E -> raise (Failure "ins always returns non empty tree")
       
  let rec member (element:elem)(set:t):bool=
    match set with
    | E -> false
    | T (col, left, el, right) ->
       if O.lt element el then member element left
       else if O.lt el element then member element right
       else true

  let get_color (st:t): color=
    match st with
    |T ( R, _, _, _) -> R
    |_ -> B
 
  let get_elem (st:t):elem option =
    match st with
    |T (_,_,e,_)->Some e
    |E -> None
       
  let  check_inv3 (p_elem:elem)(det:int)(c_elem:elem option):bool =
    match c_elem with 
    | Some el ->
       if det=0
       then O.lt el p_elem
       else O.lt p_elem el
    | None -> true
  (* 
Invariants:
1. no red node has a red child
     This is checked by comparing the parent and child colors. If both are
     red, then false, else true.
    
     See this check on line: 115

2. all paths from root have same number of black nodes
     the only case to make a special check for is the case where two children
     have different colors from each other. In order to make sure the path
     maintained the same amount of black nodes, the only thing that really
     matters is number of levels with a black node, not total amount of black
     nodes.The helper function is called on the two children, and the number 
     of levels with black nodes from each child are compared. If the numbers
     are different, then the tree violates this invariant.

     See this implementation on lines: 129, 132 -> (v1=v2)
     

3. the parent node has an element greater than every node's element in the left 
     tree and smaller than every node's element in the right tree
     
     The parent node checks to see if its value is greater than the left tree's
     element, and that the value of the right tree's element is larger using
     the helper function check_inv3. This is the first check conducted on
     the tree.
     
     See this implementation on lines: 109-111

4. root node is black
     This is checked on line: 142
  *)
  let isRedBlackTree (rb:t): bool=
    let rec checker (p:t):int*bool =
      match p with
      |E -> 0, true
      |T(col, left, el, right)->
	 let left_el = get_elem left in
	 let right_el = get_elem right in
	 if not ((check_inv3 el 0 left_el) && (check_inv3 el 1 right_el))
	 then (0, false)
	 else
	   let left_col = get_color left in
	   let right_col = get_color right in
	   (match col with
	   | R -> if left_col=R || right_col=R
	     then 0,false
	     else
	       let v1,b1 = checker left in
	       let _,b2 = checker right in
	       (v1, b1 && b2)
	   |B ->
	      (match left_col, right_col with
	      | B, B -> let v1,b1 = checker left in
			let _,b2 = checker right in
			(v1+1, b1 && b2)
	      | B, R -> let v1,b1 = checker left in
			let v2,b2 = checker right in
			(v1+1, (v1=v2) && b1 && b2)
	      | R, B -> let v1,b1 = checker left in
			let v2,b2 = checker right in
			(v1+1, (v1=v2) && b1 && b2)
	      | R, R -> let v1,b1 = checker left in
			let _ ,b2 = checker right in
			(v1, b1 && b2)
	      )
	   )
    in
    match rb with
    | E -> true
    | T(B,_,_,_) -> let _,b = checker rb in b
    | _ -> false
  
end
  
module RBTI = RedBlackTree(Int)	     

(*My testers*)
  

  
let s1 = RBTI.empty
let s2 = RBTI.insert 20 s1
let s3 = RBTI.insert 30 s2
let s4 = RBTI.insert 10 s3
let s5 = RBTI.insert 40 s4
let s6 = RBTI.insert 40 s5
let s7 = RBTI.insert 15 s6
let s8 = RBTI.insert 50 s7
let s9 = RBTI.insert 50 s8
let s10 = RBTI.insert 35 s9
  


let ir1 = RBTI.isRedBlackTree s1
let ir2 = RBTI.isRedBlackTree s2
let ir3 = RBTI.isRedBlackTree s3
let ir4 = RBTI.isRedBlackTree s4
let ir5 = RBTI.isRedBlackTree s5
let ir6 = RBTI.isRedBlackTree s6
let ir7 = RBTI.isRedBlackTree s7
let ir8 = RBTI.isRedBlackTree s8
let ir9 = RBTI.isRedBlackTree s9
  
  
let my_fail = RBTI.isRedBlackTree (RBTI.T (RBTI.B,
					   RBTI.T(
					     RBTI.R,
					     RBTI.E,
					     10,
					     RBTI.E),
					   20,
					   RBTI.T(
					     RBTI.B,
					     RBTI.E,
					     30,
					     RBTI.E)))
					      
let my_true = RBTI.isRedBlackTree (RBTI.T (RBTI.B,
					     RBTI.T(RBTI.B,
						    RBTI.T(RBTI.R,
							   RBTI.E,
							   0,
							   RBTI.E),
						    5,
						    RBTI.T(RBTI.R,
							   RBTI.E,
							   10,
							   RBTI.E)),
					   15,
					   RBTI.T(RBTI.B,
						  RBTI.T(RBTI.B,
							 RBTI.E,
							 20,
							 RBTI.E),
						  25,
						  RBTI.T(RBTI.B,
							 RBTI.E,
							 30,
							 RBTI.E))))
  						     
let fdtest1 =
  RBTI.isRedBlackTree (RBTI.T (RBTI.B,
			       RBTI.T (RBTI.B, RBTI.E, 1, RBTI.E),
			       2,
			       RBTI.T
				 (RBTI.R,
				  RBTI.T (RBTI.B, RBTI.E, 3, RBTI.E),
				  4,
				  RBTI.T (RBTI.B, RBTI.E, 5, RBTI.E))))

let fdtest2 =
  RBTI.isRedBlackTree (RBTI.T
			 (RBTI.B,
			  RBTI.T (RBTI.B,
				  RBTI.E,
				  1,
				  RBTI.E),
			  2,
			  RBTI.T (RBTI.R,
				  RBTI.T (
				    RBTI.B,
				    RBTI.E,
				    3,
				    RBTI.E),
				  4,
				  RBTI.T
				    (RBTI.R,
				    RBTI.E,
				    5,
				    RBTI.E))))
