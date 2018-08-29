open Intervals

let frac_simplify (x,y)=
  let rec euclid a b =
    if a = b then a else
      if a < b then euclid a (b-a) else
	euclid (a-b) b
  in
  let c = euclid x y in
  let (u,w) = (x/c),(y/c)
  in
  string_of_int u ^ "/" ^ string_of_int w

module Rational_comparable : (Comparable with type t = (int * int)) = struct
  type t = (int * int)
  let compare (x1, y1) (x2, y2) = compare (x1 * y2) (x2 * y1)
  let to_string (x, y) = frac_simplify (x,y)
end

module Rational_interval = Make_interval(Rational_comparable)
  
(*for compare, turn into fraction

for to_string, use frac_simplify from hwk 1*)
