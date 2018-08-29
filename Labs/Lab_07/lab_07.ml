

Problem 1:
let rec length = function 
  | [] -> 0  
  | x::xs -> 1 + length xs

Show that:
length (l @ r) = length l + length r

Using Properties:
P1: (l1 {@} l2) {@} l3 = l1 {@} (l2 {@} l3) 
P2:  x :: xs = [x] {@} xs


My Proof:

Base Case:
P([]): length ([] @ r) = Length  [] + length r

  length ([] @ r)
= length r
  by def of concat, []@r == r
= 0 + length r
  by arithmetic
= length [] + length r
  by definition of length

Inductive Case:
P((x::xs)@r): length ((x::xs) @ r) = length (x::xs) + length r
Given: length (l @ r) = length l + length r

  length ((x::xs) @ r)
= length (x :: (xs @ r))
  by understanding of lists and @
= length ([x] @ (xs @ r))
  by P2
= length x + length (xs @ r)
  by def of length
= length x + length xs + length r
  by def of length
= length (x::xs) + length r
  by def of length

(*
Problem 2:
let rec reverse l = match l with 
  | [ ] -> [ ]
  | x::xs -> reverse xs @ [x]

Show that:
 length (reverse l) = length l

Base Case:
P([]): reverse [] = []
by def of reverse, reverse [] = [] true

P[[x]]: reverse [x] = [x]

  reverse [x]
= reverse [x::[]]
  by understanding of list
= reverse [] @ [x]
  by def of reverse
= [] @ [x]
  by def of reverse
= [x]
  by understanding of list

Inductive Case:
P(length (reverse [x])): length (reverse [x]) = length [x]

  length (reverse [x])
= length (reverse [] @ [x])
  by def of reverse
= length [x]
  by def of reverse
*)    

     
