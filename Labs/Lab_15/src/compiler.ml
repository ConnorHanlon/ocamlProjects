(*open Common*)
open Driver

let () =
  let path = match Array.to_list Sys.argv with
    | [_; path] -> path
    | [] -> prerr_endline ("\x1b[1;31m" ^ "Usage: compiler [path]\x1b[0m");
      exit (-1)
    | self::_ -> prerr_endline ("\x1b[1;31m" ^ "Usage: " ^ self ^ " [path]\x1b[0m");
      exit (-1)
  in
  let src = parse_file path in
  prerr_endline (Color.blue (Src_lang_pp.string_of_program src));
  try
    print_endline (compile src);
    prerr_endline (Color.green "Compiled!")
  with
  | Failure err ->
    prerr_endline (Color.red err);
    exit (-1)
