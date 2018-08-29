open StreamModules
(*hwk5 modules*)
module type Hwk5Sig = sig
  type 'a stream
  val head: 'a stream -> 'a
  val tail: 'a stream -> 'a stream
  val take: int -> 'a stream -> 'a list
  val zip: ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream
    
  val from: int -> int stream 
  val nats: int stream
  val cubes_from: int -> int stream 
  val cubes_from_zip: int -> int stream 
  val cubes_from_map: int -> int stream 
  val drop: int -> 'a stream -> 'a stream 
  val drop_until: ('a -> bool) -> 'a stream -> 'a stream 
  val sum_positive_prefix: int stream -> int 
  val primes: int stream 
end

module Hwk5(S: StreamSig) : Hwk5Sig = struct
  type 'a stream = 'a S.t
  let tail = S.tail
  let head = S.head
  let take = S.take
  let zip = S.zip

  let rec from (n:int): 'a stream = 
    S.Cons ( n,  S.delay (fun () -> from (n+1)))

  let nats = from 1
    
  let cube (i:int) = i * i * i
    
  let rec cubes_from (i:int): int stream =
    Cons (cube i, S.delay (fun () -> cubes_from (i+1)))
      
  let rec intStream (i:int): int stream =
    S.Cons (i, S.delay (fun () -> intStream (i+1)))
      
  let cubes_from_zip (i:int): 'a stream =
    let is = intStream i in
    let zip_cube (a:int)(_):int = cube a in
    S.zip zip_cube is is
      
  let rec cubes_from_map (i:int): int stream =
    let is = intStream i  in
    S.map cube is
      
  let rec drop (i:int) (st: 'a stream): 'a stream =
    match i, st with
    | 0, _ -> st
    | _, S.Cons (hd, tl) -> drop (i-1) (S.demand tl)
       
  let rec drop_until (f: 'a -> bool) (st:'a stream): 'a stream =
    match st with
    | S.Cons (hd, tl) ->
       match (f hd) with
       | true -> st
       | _ -> drop_until f (S.demand tl)
	  
  let rec foldr (f: 'a -> 'b S.lazee -> 'b) (st: 'a stream): 'b =
    match st with
    | S.Cons (hd, tl) ->
       let rest = S.delay ( fun () -> foldr f (S.demand tl))
       in
       f hd rest
	 
  let sum_positive_prefix (is:int stream):int =
    let my_sum_pos (i1:int) (rest: 'b S.lazee ): 'b =
      if i1 > 0 then i1 + (S.demand rest)
      else 0
    in
    foldr my_sum_pos is
      
  let sift (i:int)(istream:int stream): int stream =
    let is_mul (i1:int):bool = if i1 mod i = 0 then false
      else true
    in
    S.filter is_mul istream

  let rec sieve (st:int stream): int stream =
    match st with
    |S.Cons (hd, tl) ->
       let sifted = sift hd (S.demand tl)
       in
       S.Cons(hd, S.delay( fun () -> (sieve sifted) ))
	 
  let primes = sieve (from 2)
    
end
  
