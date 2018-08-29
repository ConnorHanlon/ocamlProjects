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
;;
let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)
;;
let d1 = "../../public-class-repo/Homework/Files/words-small.txt";;
let d2 = "../../public-class-repo/Homework/Files/words-google-10000.txt";;

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
;;

let rem_punc flnm = split (fun x -> if x = ' ' ||  x = '\n' then true else false) flnm;;
    
let sep xs = if (List.length xs) = 4 || (List.length xs) = 6 then true else false;;

let sub_four s = String.sub s 1 4;;

let sixes_and_fours sep xs = List.filter sep e;;


      
let answers (flnm: string) : char list =
  let x = read_file flnm
  in
  let e = rem_punc x
  in
  (* let sep xs = if (List.length xs) = 4 || (List.length xs) = 6 then true else false*)
  let only_six_four = sixes_and_fours sep e 
  in
  let imploded = List.map implode only_six_four
  in (sixes, fours) = List.partition (fun x -> if String.length x = 6 then true else false) imploded
;;
(*
let f_u (flnm: string) : chaar list =
  let (sixxxxes, fours) = List.partition (fun x -> if (String.length x) = 6 then true else false) (List.map implode (sixes_and_fours sep (rem_punc (read_file flnm)));;
							    
*)
