let even x = if x mod 2 = 0 then true else false

let rec euclid x y=
  if x = y then x else
    if x < y then euclid x (y-x) else
      euclid (x-y) y

let frac_simplify (x,y)=
  let b = euclid x y in ((x/b),(y/b))

let rec max : int list -> int = fun xs ->
  match xs with
  |[] -> raise(Failure "Input list must not be empty")
  |x::[] -> x
  |x::y::rest -> if x < y then max (y::rest) else max (x::rest)

let rec take x xs = 
  if x<=0 then [] else
  match xs with
  |[]->[]
  |y::[]->y::[]
  |y::rest-> let b = x-1 in y::(take b rest)

