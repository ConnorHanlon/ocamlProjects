(*let terminator b1 b2 =
  match b1 with
  | true -> b2
  | _ -> false 

let rec ands (bl : bool list) : bool =
  List.fold_right terminator bl true

*)

let rec ands (bl : bool list): bool =
  match bl with
  | [] -> true
  | x :: xs ->
     (match x with
     | true -> ands xs 
     | _ -> x
     )
