let all_evens xs =
  let is_even x = if x mod 2 = 0 then true else false
  in List.filter is_even xs

let increment_all xs =
  let inc x = x+1
  in List.map inc xs

let max_fold xs=
  let max_x x y = if x<y then y else x
  in let min_x xs =
   match xs with
   | [] -> 0
   | x :: [] -> x
   | x::y::rest-> if x<y then x else y
  in List.fold_left max_x (min_x xs) xs

let sum_prod xs =
  let sum_all = List.fold_left (+) 0 xs
  in let prod = List.fold_left ( * ) 1 xs
  in (sum_all, prod)

let split f xs =
    let acc = (f,[],[])
    in
    let sf (f, sub, cur) x =
      if f x = false
        then (f, sub, x::cur)
      else if f x && cur = []
        then (f, List.rev []::sub, cur)
      else (f, List.rev cur :: sub, [])

    in
    let (f, sub, cur) = List.fold_left sf acc xs
    in
    if cur = []
      then List.rev sub
    else List.rev(List.rev cur::sub)
