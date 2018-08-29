
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

let p1 = "Hello world!\n\n How are you today? \t\t I hope all is well. "

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

let explode (s: string) : char list =
  let l = String.length s
  in
  let rec f i = 
    if i = l then [] else s.[i] :: f (i+1)
  in f 0

let format str fnum =
  let char_list = explode str
  (*List.filter (fun x -> if x = ' ' || x = '\n' || x = '\t' then false else true) x*)
  in
  let no_newlines = split (fun x -> if x = ' ' || x = '\n' || x = '\t' then true else false) char_list
  in
  let no_spaces = List.filter (fun x -> if x = [] then false else true) no_newlines
  in
  let str_list = List.map (fun x -> implode x) no_spaces
  in
  let concatted = String.concat "\n" str_list
  in
  print_endline concatted
  
