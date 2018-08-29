let length xs =
  let add_one x y = x + 1
    in List.fold_left add_one 0 xs

let andf bls =
  let det_truthy x = if x then true else false
  in List.for_all det_truthy bls 


let orf bls =
  let det_falsey x = if x = false then true else false
      in List.for_all det_falsey bls

let is_elem a xs =
  let p x = if x = a then true else false
      in List.exists p xs

let list_reverse xs =
  let f lst x = x::lst
    in List.fold_left f [] xs
