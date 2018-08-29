let read_file (file_name: string) : char list =
  let ic = open_in file_name
  in
  let rec read_chars ic =
    try
      let next_char = input_char ic
      in next_char :: read_chars ic
    with
      _ -> []
  in read_chars ic

let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)

let d1 = "../../public-class-repo/Homework/Files/words-small.txt"
let d2 = "../../public-class-repo/Homework/Files/words-google-10000.txt"

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


let rem_front_back sixes =
  let rem_front = List.tl sixes
  in
  let rev_one = List.rev rem_front
  in
  let rem_back = List.tl rev_one
  in List.rev rem_back

let sep xs = if (List.length xs) = 4 || (List.length xs) = 6 then true else false


let answers (flnm: string) =
  let x = read_file flnm
  in
  let e = split (fun x -> if x = ' ' || x = '\n' then true else false) x
  in
  let only_six_four = List.filter sep e
  in
  let (sixes, fours) =  List.partition (fun strs -> if List.length strs = 6 then true else false) only_six_four
  in
  let (six_imploded, four_imploded) = (List.map implode sixes, List.map implode fours)
  in List.filter (fun str -> if List.mem (String.sub str 1 4) four_imploded = true then true else false) six_imploded

let pretty_answers sixes = 
  List.map (fun six_str -> ((String.sub six_str 1 4), six_str)) sixes
