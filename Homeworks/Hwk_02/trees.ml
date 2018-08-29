(*PART A*)
type 'a tree = Leaf of 'a
             | Fork of 'a * 'a tree * 'a tree


let t1 = Leaf 5
let t2 = Fork (3, Leaf 3, Fork (2, t1, t1))
let t3 = Fork ("Hello", Leaf "World", Leaf "!")
let t4 = Fork (7, Fork (5, Leaf 1, Leaf 2), Fork (6, Leaf 3, Leaf 4))


let rec t_size (t:'a tree): int =
  match t with
  |Leaf _ -> 1
  |Fork (_, tl, tr) -> 1 + t_size (tl) + t_size (tr)

let rec t_sum (t:int tree): int =
  match t with
  |Leaf i -> i
  |Fork (i, tl, tr) -> i + t_sum tl + t_sum tr

let rec t_charcount  (t: string tree): int =
  match t with
  |Leaf s -> (String.length s)
  |Fork (s, tl, tr) -> (String.length s) + (t_charcount tl) + (t_charcount tr)

let rec t_concat (t:string tree) : string =
  match t with
  |Leaf s -> s
  |Fork (s, tl, tr) -> s ^ t_concat tl ^ t_concat tr


(*PART B*)
     
(*Options are an Ocaml standard type that can be either None, or Some 'a, according to ocaml library.*)
let t5 : string option tree =
  Fork (Some "a",
        Leaf (Some "b"),
        Fork (Some "c",
              Leaf None,
              Leaf (Some "d")))

let t7 = Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None))
let t8 = Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d")))

let rec t_opt_size (t: 'a option tree): int =
  match t with
  |Leaf (Some _) -> 1
  |Leaf (None) -> 0
  |Fork (Some _, tl, tr) -> 1 + t_opt_size tl + t_opt_size tr
  |Fork (None, tl, tr) -> t_opt_size tl + t_opt_size tr


let rec t_opt_sum (t:'a option tree): int =
  match t with
  |Leaf (Some x) -> x
  |Leaf (None) -> 0
  |Fork (Some x, tl, tr) -> x + t_opt_sum tl + t_opt_sum tr
  |Fork (None, tl, tr) -> t_opt_sum tl + t_opt_sum tr

let rec t_opt_charcount (t:'a option tree): int =
  match t with
  |Leaf (Some s) -> String.length s
  |Leaf (None) -> 0
  |Fork (Some s, tl, tr) -> String.length s + t_opt_charcount tl + t_opt_charcount tr
  |Fork (None, tl, tr) -> t_opt_charcount tl + t_opt_charcount tr

let rec t_opt_concat (t:'a option tree): string =
  match t with
  |Leaf (Some s) -> s
  |Leaf (None) -> ""
  |Fork (Some s, tl, tr) -> s ^ t_opt_concat tl ^ t_opt_concat tr
  |Fork (None, tl, tr) -> t_opt_concat tl ^ t_opt_concat tr


(*PART C*)
     
let rec tfold (l:'a->'b) (f:'a->'b->'b->'b) (t:'a tree): 'b =
  match t with
  |Leaf v -> l v
  |Fork (v, t1, t2) -> f v (tfold l f t1) (tfold l f t2)

let tf_size (t:'a tree): int =
  let l _ = 1
  in
  let f (a:_)(b:_)(c:_):int = 1 + b + c
  in
  tfold l f t

let tf_sum (t:int tree): int =
  let l x = x
  in
  let f x y z = l x + y + z
  in
  tfold l f t

let tf_charcount (t:string tree): int =
  let l s = String.length s
  in
  let f (s:_) (a:_) (b:_):int = l s + a + b
  in
  tfold l f t


let tf_concat (t:string tree): string=
  let l s = s
  in
  let f (s:string) (left:string) (right:string):string = s ^ left ^ right
  in
  tfold l f t


let tf_opt_size (t:'a option tree): int=
  let l opt =
    match opt with
    |None -> 0
    |Some _ ->1
  in
  let f (x:_) (left:int)(right:int):int = 1 + left + right
  in
  tfold l f t

let tf_opt_sum (t:int option tree): int=
  let l (opt:int option):int = 
    match opt with
    |None -> 0
    |Some x -> x
  in
  let f (opt:int option)(left:int)(right:int):int = (l opt) + left + right
  in
  tfold l f t

let tf_opt_charcount (t:string option tree):int =
  let l (opt:string option):int =
    match opt with
    |None -> 0
    |Some s -> String.length s
  in
  let f (opt: string option)(left:int)(right:int):int = (l opt) + left + right
  in
  tfold l f t

let tf_opt_concat (t:string option tree): string =
  let l (opt:string option):string=
    match opt with
    |None -> ""
    |Some s -> s
  in
  let f (opt:string option)(left:string)(right:string):string = (l opt)^left^right
  in
  tfold l f t


    
(*Part D*)

type 'a btree = Empty | Node of 'a btree * 'a * 'a btree

let t6 = Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Empty))

  
(*Compare x y -> returns 0 if they are equal, negative if x less than y
  or positive if x is greater than y*) 
let rec bt_insert_by (cmp: 'a->'a->int) (x:'a)(t:'a btree): 'a btree=
  match t with
  | Empty -> Node (Empty, x, Empty)
  | Node (tl, y, tr) ->
     if cmp x y < 0
     then  Node(bt_insert_by cmp x tl, y, tr)
     else  Node(tl, y, bt_insert_by cmp x tr)

let rec bt_elem_by (f: 'a->'b->bool)(x:'b)(t:'a btree): bool=
  match t with
  |Empty -> false
  |Node (tl, y, tr) ->
     if f x y = false
     then bt_elem_by f x tl || bt_elem_by f x tr
     else true

(*Personal Note: the cons operator takes 'a as first argument, 'a list as second, and adds 'a to front of list
  the append operator (@) appends a 'a list to another 'a list, and does not append an individual element 'a*)
       
let rec bt_to_list (t:'a btree): 'a list=
  match t with
  |Empty -> []
  |Node (tl, y, tr)->
     let right = y::(bt_to_list tr)
     in
     (bt_to_list tl)@right

(*x is the accumulator, f is the function that is applied to every 'a in the btree, and returns a 'b value*)       
let rec btfold (x:'b) (f:'b-> 'a-> 'b-> 'b) (t:'a btree): 'b=
  match t with
  |Empty -> x
  |Node (tl, y, tr) ->
     f (btfold x f tl) y (btfold x f tr)



let btf_elem_by (f:'b-> 'a-> bool)(x:'b)(t:'a btree): bool =
  match t with
  |Empty -> false
  |Node (tl, y, tr) ->
     let f_elem (a1: bool) (a2:'a) (a3: bool): bool = 
       a1 || (f x a2) || a3
     in
     f_elem (btfold false f_elem tl) y (btfold false f_elem tr)


let btf_to_list (t:'a btree): 'a list=
  match t with
  |Empty -> []
  |Node (tl, y, tr) ->
     let f_to_list (a1:'b)(a2:'a)(a3:'b): 'b =
       let right = a2::a3
       in a1 @ right
     in
     f_to_list (btfold [] f_to_list tl) y (btfold [] f_to_list tr)
					       
(*
btf_insert_by:

btfold has a 'a btree as an input and requires an output of 'b. If it
was going to be used to insert a node, it would be difficult to use btfold
to return a btree tree or modify it. Btfold is used to work over a structure 
condense it into one data type, or "fold" something larger into one type.
*)
