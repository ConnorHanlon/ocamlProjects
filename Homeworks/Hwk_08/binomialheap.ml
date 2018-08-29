open Ordered


module type BinomialHeapSig = sig
  type elem
  type tree = Node of int * elem * tree list
  type t = tree list
  val empty: t
  val isEmpty: t->bool
  val insert: elem -> t -> t
  val merge: t -> t -> t
  val findMin: t -> elem
  val deleteMin: t -> t
  val isBinomialTree: tree -> bool
  val isBinomialHeap: t -> bool
  val findMinDirect: t -> elem
end

    
module BinomialHeap (O:OrderedSig):(BinomialHeapSig with type elem = O.t)=struct
  type elem = O.t
  type tree = Node of int * elem * tree list
  type t = tree list      
  let empty = []
  let isEmpty (heap:t):bool =
    match heap with
    |[] -> true
    | _ -> false
       
  let link (t1: tree)(t2: tree): tree=
    match t1, t2 with
    | Node (rank1, e1, ts1), Node (rank2, e2, ts2) ->
       if O.leq e1 e2 then Node (rank1+1, e1, t2::ts1)
       else Node (rank1+1, e2, t1::ts2)
	 
  let rank (sometree:tree):int=
    match sometree with
    |Node (r, _, _) -> r

  let get_heap (st:tree): t =
    match st with
    | Node (_, _ , hp) -> hp
       
  let rec insert_tree (t1:tree)(heap:t):t=
    match  heap with
    |  [] -> [t1]
    | Node (rank2, element , tsl)::rest_trees ->
       if (rank t1)<rank2 then t1::heap
       else insert_tree (link t1 (Node (rank2, element, tsl))) rest_trees
	 
  let rec insert (e:elem)(heap:t):t=
    insert_tree (Node (0, e, [])) heap
      
  let rec merge (heap1:t)(heap2:t):t =
    match heap1, heap2 with
    | heap1, [] -> heap1
    | [], heap2 -> heap2
    | Node (rank1, elem1, hd_heap1)::rest1, Node (rank2, e2, hd_heap2)::rest2->
       if  rank1<rank2
       then (Node (rank1, elem1, hd_heap1)):: (merge rest1 heap2)
       else if rank2<rank1
       then (Node (rank2, e2, hd_heap2)):: (merge heap1 rest2)
       else
	 let linked =
	   (link (Node (rank1, elem1, hd_heap1)) (Node (rank2, e2, hd_heap2)))
	     in insert_tree linked (merge rest1 rest2)
	 
  let rec removeMinTree (heap:t): tree * t =
    match heap with
    | [] -> raise (Failure "heap empty")
    | hd::[] -> (hd, [])
    | (Node (rank1, elem1, ts1))::rest ->
       let (Node (rank2, e2, ts2), heap2) = removeMinTree rest in
       if O.leq elem1 e2 then
	 Node (rank1, elem1, ts1) , rest
       else
	 Node (rank2, e2, ts2) , (Node (rank1, elem1, ts1))::heap2
	 
  let findMin (heap:t): elem =
    match removeMinTree heap with
    | Node (_, e, _), _ -> e
       
  let deleteMin (heap:t):t =
    match removeMinTree heap with
    | Node (_,_,rest_tree1), heap2 ->
       merge  (List.rev rest_tree1) heap2

  let rec raise_two (n:int):int =
    match n with
    | 0 -> 1
    | any -> 2 * raise_two (n-1)
      
  let rec check_inv2 (p:tree):bool=
    let rec helper (p_rank:int)(heap:t):bool =
      match p_rank, heap with
      |p_rank, [] -> p_rank=0
      |p_rank, hd::rest ->
	 let hd_rank = rank hd in
	 (p_rank = hd_rank+1) && helper hd_rank rest
    in
    match p with
    |Node (p_r, _ , p_h) -> helper p_r p_h

       
       
         (*	 Binary Tree invariants:
1. binomial tree of rank 0 is a single node
		 My implementation:
if a node is of rank 0, there must be no children in its heap and therefore 
only consist of one node, itself. This was checked with if (rank parent)=0
in two of the match cases in the binhelper function. If the rank of the
parent is zero but the if heap passed to binhelper from the parent has more 
elements, this invariant is violated. 

		 See this on lines: 159,163

2. binomial tree of rank r+1 formed from two linked binomial trees of 
   rank r, and making one the left child of the other
		 My implementation:
The base tree of rank 0 has no children. A tree of rank 1 has two trees of 
rank 0. A tree of rank 2 has two trees of rank 1, each themselves comprised
of two trees of rank 0. A pattern arises where the first child of the 
largest rank makes the subsequent children trees the of the first child.
This is due to the recurrent nature of the invariance.
if r = (r-1) + (r-1) with (r-1) child of (r-1) in
r+1 = r + r -> r+1 = r + (r-1 + r-2 ... 0). With this relationship noted,
I created an external helper function called check_inv2 that checks to
see that the parent rank equals the first, highest ranking child minus one 
rank, then called the internal helper function on the rest of the children
in the parent tree's heap to account for the recurrence. 

		 The check_inv2 function is called on line: 173

3. tree of rank r has 2^r nodes
		 My Implementation:
A tree of rank 0 has only one node. This holds for 2^R=1, and also checked in 
invariant 1. This is my base case. What was implemented was recursion to add
up each node from the base node to the root of the tree. Once all nodes have 
been traversed, an external helper function raise_two, which raises 2 to the 
power of the input, was called with the rank of the tree being checked. The
return from raise_two is compared to the amount of nodes returned. 

		 The accumulation of nodes seen on lines: 157,161,168

		 The invariant 3 check on line: 172

4. tree of rank r has a heap with r children
		 My Implementation:
simply check to see if the rank of the parent tree has a heap with rank
elements.
		 See invariant 4 check on line: 172
	 *)
    
  let isBinomialTree (st:tree): bool=
    let rec binhelper (parent:tree)(heap:t): int * bool =
      match heap with
      | [] -> 1, true
      | child :: [] ->
	 if (rank parent)=0 && (rank parent) <= (rank child)
	then (0, false)
	  else  (2, true)
      | child1::child2::rest ->
	 if (rank parent)=0 || (rank parent)<=(rank child1)
	 then (0, false)
	 else
	   let (v1, b1) = binhelper child1 (get_heap child1) in
	   let (v2, b2) = binhelper parent (child2::rest) in
	   (v1+v2, b1&&b2)
    in match st with
    | Node (st_rank, _ , st_heap)->
       let (value, b) = binhelper st st_heap
       in (raise_two st_rank)=value && b && (st_rank= (List.length st_heap))
	   && (check_inv2 st)
   
(*
Binary Heap invariants:
  1. No two trees have same rank-
  This check was implemented alonside
  the invariant 2 check. See below.

  2. Stored in order of increasing rank
  The heap is traversed checking the head and tail trees.
  If a tail tree has a rank equal to or greater than the
  rank of the head tree, then invariants 1 and 2 violated.

  See this implementation on line: 205

  3. Every tree in binomial heap is indeed a binomial tree

  Every tree inside of the heap calls isBinomialTree to make sure it is a
  binomial tree satistying heap property. If one does not, then the heap
  is not a binomial heap.

  See this implementation on lines: 199, 201
*)	 
  let rec isBinomialHeap (heap:t): bool=
    match heap with
    | [] -> true
    | head::[] -> isBinomialTree head
    | head1::head2::rest ->
       if not (isBinomialTree head1 && isBinomialTree head2) then false
       else
	 match head1, head2 with
	 | Node (h1_rank, _, h1_heap), Node (h2_rank, _, h2_heap) ->
	    if h1_rank > h2_rank then false else
	      isBinomialHeap (head2::rest)

  let rec findMinDirect (heap:t):elem =
    match heap with
    | [] -> raise (Failure "heap empty")
    | Node (rank1, e1, ts1)::[] -> e1
    | Node (rank1, e1, ts1)::rest->
       let small = findMinDirect rest in
       if O.lt e1 small then e1
       else small
end
  
	   
module BHI = BinomialHeap(Int)
let t1 = BHI.isBinomialTree(BHI.Node (0,10, []))
  (*
  let h1 = BHI.empty
  let h2 = BHI.insert 20 h1
  let h3 = BHI.insert 30 h2
  let h4 = BHI.insert 10 h3
  let h5 = BHI.insert 40 h4

  let m1 = BHI.findMin h5

  let h6 = BHI.deleteMin h5

  let m2 = BHI.findMin h6

  let m3 = BHI.findMinDirect h6

  let h7 = BHI.insert 10 h6
  let h8 = BHI.insert 20 h7
  let h9 = BHI.insert 50 h8

 
  let h10 = BHI.merge h8 h9
   

  let m4 = BHI.findMin h10
  let m5 = BHI.findMinDirect h10
    
  let h11 = BHI.deleteMin h10
  let h12 = BHI.deleteMin h11
  let h13 = BHI.insert 60 h10
  let h14 = BHI.insert 55 h13
  let h15 = BHI.insert 65 h14
  let h16 = BHI.merge h14 h10
    
    

  let m6 = BHI.findMin h12
  let m7 = BHI.findMinDirect h12
  let isBin1 = BHI.isBinomialHeap h1
  let isBin2 = BHI.isBinomialHeap h2
  let isBin3 = BHI.isBinomialHeap h3
  let isBin4 = BHI.isBinomialHeap h4
  let isBin5 = BHI.isBinomialHeap h5
  let isBin6 = BHI.isBinomialHeap h6
  let isBin7 = BHI.isBinomialHeap h7
  let isBin8 = BHI.isBinomialHeap h8
  let isBin9 = BHI.isBinomialHeap h9
    
  let isBin10 = BHI.isBinomialHeap h10
    let isBin15 = BHI.isBinomialHeap h15
    
  let extra_test = BHI.findMin (BHI.insert 4 (BHI.insert 8 (BHI.insert 6 (BHI.insert 2 BHI.empty))))
   
 
*)
