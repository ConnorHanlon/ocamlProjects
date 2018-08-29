let circle_circum_v1 : float -> float = fun r -> 2. *. 3.1415 *. r;;

let circle_circum_v2 r = let pi = 3.1415 in 2. *. pi *. r;;

let rec product xs =
  match xs with
  | [] -> 0
  | x::[] -> x
  | x::rest -> x * product rest
;;

let rec sum_sqrdiffs xs =
  match xs with 
  | [] -> 0
  | x::[] -> 0
  | x1::x2::rest -> (x1-x2) * (x1-x2) + sum_sqrdiffs (x2::rest)
;;

let distance (x,y) (a,b) =
  let unsqx = (a-.x) *. (a-.x)
  in let unsqry = (b-.y)*. (b-.y)
     in
     sqrt(unsqx +. unsqry)
;;

let triangle_perimeter (x1,x2) (y1,y2) (z1, z2)=
  let s1 = distance (x1,x2) (y1,y2)
  in let s2 = distance (y1,y2) (z1,z2)
     in let s3 = distance (z1,z2) (x1,x2)
	in s1 +. s2 +. s3
;;
