(* The code below is from Professor Eric Van Wyk *)

(* Types and functions for lazy values  *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a 
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

     (* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee


let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s: 'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let rec take (n:int) (s : 'a stream) : ('a list) =
 match n, s with
 | 0, _ -> []
 | _, Cons (v, tl) -> v :: take (n-1) (demand tl)

     
let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (hd1, tl1), Cons (hd2, tl2) ->
     Cons (f hd1 hd2, delay (fun () -> zip f (demand tl1) (demand tl2)))
       
    
let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (hd, tl) ->
     Cons (f hd, delay (fun () -> map f (demand tl)))

let rec from n = 
  (* print_endline ("step " ^ string_of_int n) ; *)
  Cons ( n, 
         delay (fun () -> from (n+1) )
       )      

let nats = from 1

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) -> 
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd 
     then Cons (hd, rest)
     else demand rest



       
(* Code below is from Connor Hanlon *)
let cube (i:int): int = i * i * i
    
let rec cubes_from (i:int): int stream =
  Cons (cube i, delay (fun () -> cubes_from (i+1)))

let my_nats i  = cubes_from i
  
       
let rec intStream (i:int): int stream =
  Cons (i, delay (fun () -> intStream (i+1)))
    
let rec cubes_from_map (i:int): int stream =
  let s = intStream i  in
  map cube s 

let cubes_from_zip (i:int): 'a stream =
  let s = intStream i in
  let zip_cube (a:int)(_):int = cube a in
  zip zip_cube s s

let rec drop (i:int) (s: 'a stream): 'a stream =
  match i, s with
  | 0, _ -> s
  | _, Cons (hd, tl) -> drop (i-1) (demand tl)

let rec drop_until (f: 'a -> bool) (s:'a stream): 'a stream =
  match s with
  | Cons (hd, tl) ->
     match (f hd) with
     | true -> s
     | _ -> drop_until f (demand tl)
    
 

(* 

Foldr takes as an input a function that works on the head of a stream, 
and the lazee tail. The tail is not evaluated until it needs to be,
as the function will first determine what to do based on the 
head of the stream. If required, the function as input to the 
fold will demand the evaluation of the rest.

*)   
let rec foldr (f: 'a -> 'b lazee -> 'b) (s: 'a stream): 'b =
  match s with
  | Cons (hd, tl) ->
     let rest = delay ( fun () -> foldr f (demand tl))
     in
     f hd rest

let and_fold (b:bool stream):bool =
  let my_and (b1:bool) (rest: 'b lazee): 'b = if b1 then (demand rest)
    else false
  in
  foldr my_and b
  
let sum_positive_prefix (i:int stream):int =
  let my_sum_pos (i1:int) (rest:'b lazee): 'b =
    if i1 > 0 then i1 + (demand rest)
    else 0
  in
  foldr my_sum_pos i

(*
 Removes every number from int stream that is a 
multiple of i
 *)
let sift (i:int)(istream:int stream): int stream =
  let is_mul (i1:int):bool = if i1 mod i = 0 then false
    else true
  in
  filter is_mul istream


let rec sieve (s:int stream): int stream =
  match s with
  |Cons (hd, tl) ->
     let sifted = sift hd (demand tl)
       in
     Cons(hd, delay( fun () -> (sieve sifted) ))

  
(* My Testing Area*)
(*
let t1 = head(tail (cubes_from_map 5))
let t2 = head(tail (cubes_from 5))
let t3 = head(tail (cubes_from_zip 5))
  
let ns = zip ( - ) (from 1000) (cubes_from 1)
let are_positive nums = map (fun n -> n > 0) nums
 
let ns_positive = are_positive ns
let my_cons = Cons (false, delay (fun () -> ns_positive))
  
  
let t4 = take 15 ns
let t5 = take 16 ns_positive

let t6 = head (tail (drop 3 (cubes_from 3)))
let t7 = head (drop_until (fun v -> v > 35) nats)

let primes = sieve (from 2)

let () =
  assert (t1 = 216);
  assert (t2 = 216);
  assert (t3 = 216);
  assert (t6 = 343);
  assert (t7 = 36);
*)
