(*Connor Hanlon*)

(*circle_circum_v1 is one line. It is easy to understand the math 
  and the argument is intuitive r for radius.*)
(*circle_circum_v1 radius = 2.0 *. 3.14159 *. radius *)
let circle_circum_v1 radius = 2.0 *. 3.1415 *. radius

(*Number of lines is 1, no indentation, not easy to see where nested let is*)
(*Change: let on new line, in and math on new line:
Let circle_circum_v2 radius =
   let pi = 3.14159
   in 2.0 *. pi *. radius
*)
let circle_circum_v2 radius =
  let pi = 3.1415
  in 2.0 *. pi *. radius

(*I didn't get any warnings, because I matched the [] case.*)			(*Change: delete second match as it is not needed *)		 
let rec product xs =
  match xs with
  | [] -> 1
  | x::rest -> x * product rest

(*Does not use raise construct, and did not use @ but the :: operator*)
(*Change: delete match 1 and 2, replace with 
| x::[] -> raise(Failure "sum_sqrdiffs input list needs at least two elements")*)
let rec sum_sqrdiffs xs =
  match xs with
  | [] -> 0
  | x1::[]-> raise(Failure "sum_sqrdiffs list needs at least two elements")
  | x1::x2::rest -> (x1-x2) * (x1-x2) + sum_sqrdiffs (x2::rest)

(*triangle_perimeter uses distance as helper.
Changes: unsqrx to xsqrd and unsqrdy to ysqrd,
put sqrt(xsqrd +. ysqrd) on same line as second in*)
let distance (x,y) (a,b) =
  let xsqrd = (a-.x) *. (a-.x)
  in let ysqrd = (b-.y)*. (b-.y)
     in
     sqrt(xsqrd +. ysqrd)

(*No changes to input parameters as they allow the function to be easily read*)
let triangle_perimeter (x1,x2) (y1,y2) (z1, z2)=
  let s1 = distance (x1,x2) (y1,y2)
  in let s2 = distance (y1,y2) (z1,z2)
     in let s3 = distance (z1,z2) (x1,x2)
	in s1 +. s2 +. s3
