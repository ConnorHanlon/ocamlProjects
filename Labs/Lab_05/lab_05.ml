(*Connor Hanlon, SUNG HYUN JANG*)

(*My naming convention makes my code harder to read, which was definitely apparent when I tried describing my own function and having a harder time remembering what exactly I did.
 I changed my variable names to names that make more sense to readers. I specified argument and return types for each function. I also simplified and compacted code so that no line is
unnecessarily longer than 80 characters.*)

type words_list = 'a list
type two_d_list = 'a list list
  
let split (f:'a -> bool) (xs: 'a list): two_d_list =
    let accumulator = (f,[],[])
    in
    let spliterator (f, sublist, cur) x =
      if f x = false
        then (f, sublist, x::cur)
      else if f x && cur = []
        then (f, List.rev []::sublist, cur)
      else (f, List.rev cur :: sublist, [])
    in
    let (f, sublist, cur) = List.fold_left spliterator accumulator xs
    in
    if cur = []
      then List.rev sublist
    else List.rev(List.rev cur::sublist)


let extract_four_sixes (xs:'a list): bool =
  if (List.length xs) = 4 || (List.length xs) = 6 then true
  else false

(*Helper function to answers and format*)
let rem_spec_chars (x: char): bool =
  match x with
    (' ' | '\n' | '\t')-> true
  |_ -> false
    
(*Answers uses previously defined sep function*)
    
let answers (flnm: string): words_list =
  let file_char_list = read_file flnm
  in
  let no_fluff = split rem_spec_char file_char_list
  in
  let only_six_four = List.filter extract_four_sixes sep no_fluff
  in
  let get_sixes (str: string):bool = if List.length str = 6 then true else false
  in
  let (sixes, fours) =  List.partition get_sixes only_six_four
  in
  let (six_imploded, four_imploded) = (List.map implode sixes, List.map implode fours)
  in
  let find_comp (str:string):bool = List.mem (String.sub str 1 4) four_imploded
    in List.filter find_comp six_imploded

  
let pretty_answers (sixes: 'a list): ('a * 'a) list = 
  List.map (fun six_str -> ((String.sub six_str 1 4), six_str)) sixes


    
(*Was not completed*)
let format (str : 'a) (fnum: int) : string  =
  let char_list = explode str
  in
  let no_newlines = split rem_spec_chars char_list
  in
  let no_spaces = List.filter (fun x -> if x = [] then false else true) no_newlines
  in
  let str_list = List.map (fun x -> implode x) no_spaces
  in
  let concatted = String.concat "\n" str_list
  in
  print_endline concatted

  
