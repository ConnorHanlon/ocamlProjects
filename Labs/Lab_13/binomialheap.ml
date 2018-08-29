
module type OrderedSig = sig
  type t
  val eq: t -> t -> bool
  val lt: t -> t -> bool
  val leq: t -> t -> bool
end

module Int: (OrderedSig with type t=int) = struct
  type t = int
  let eq (i1:int)(i2:int):bool = (i1=i2)
  let lt (i1:int)(i2:int):bool = (i1<i2)
  let leq (i1:int)(i2:int):bool = (i1<i2) || (i1=i2)
end
  

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
end
  
    
module BinomialHeap (O:OrderedSig): (BinomialHeapSig with type elem = O.t) = struct
  type elem = O.t
  type tree = Node of int * elem * tree list
  type t = tree list      (*heap*)
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
  let rec insert_tree (t1:tree)(heap:t):t=
    match  heap with
    |  [] -> [t1]
    | Node (rank2, elem , tsl)::rest_trees ->
       if (rank t1)<rank2 then t1::heap
       else insert_tree (link t1 (Node (rank2, elem, tsl))) rest_trees	 
  let rec insert (e:elem)(heap:t):t=
    insert_tree (Node (0, e, [])) heap     
  let rec merge (heap1:t)(heap2:t):t =
    match heap1, heap2 with
    | [], _ -> heap2
    | _, [] -> heap1
    | Node (r1, e1, hd_ts1)::rest1, Node (r2, e2, hd_ts2)::rest2 ->
       if r1<r2 then (Node (r1, e1, hd_ts1)):: (merge rest1 heap2)
       else if r2<r1 then (Node (r2, e2, hd_ts2)):: (merge heap1 rest2)
       else insert_tree (link (Node (r1, e1, hd_ts1)) (Node (r2, e2, hd_ts2))) (merge rest1 rest2)	 
  let rec removeMinTree (heap:t): tree * t =
    match heap with
    | [] -> raise (Failure "heap empty")
    | hd::[] -> (hd, [])
    | (Node (r1, e1, ts1))::rest ->
       let (Node (r2, e2, ts2), heap2) = removeMinTree rest in
       if e1<=e2 then ((Node (r1, e1, ts1)), rest)
       else (Node (r2, e2, ts2)), (Node (r1, e1, ts1))::heap2	 
  let findMin (heap:t): elem =
    match removeMinTree heap with
    | Node (_, e, _), _ -> e    
  let deleteMin (heap:t):t =
    match removeMinTree heap with
    | Node (_,_,rest_tree1), heap2 ->
       merge  rest_tree1 heap2
end
  
	   
  module BHI = BinomialHeap(Int)
  let h1 = BHI.empty
  let h2 = BHI.insert 20 h1
  let h3 = BHI.insert 30 h2
  let h4 = BHI.insert 10 h3
  let h5 = BHI.insert 40 h4

  let m1 = BHI.findMin h5

  let h6 = BHI.deleteMin h5

  let m2 = BHI.findMin h6

  let extra_test = BHI.findMin (BHI.insert 4 (BHI.insert 8 (BHI.insert 6 (BHI.insert 2 BHI.empty))))
    
