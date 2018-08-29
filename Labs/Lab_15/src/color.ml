(** Colored string functions. *)

let color n s = "\x1b[1;3" ^ string_of_int n ^ "m" ^ s ^ "\x1b[0m"

let red = color 1
let green = color 2
let blue = color 4
