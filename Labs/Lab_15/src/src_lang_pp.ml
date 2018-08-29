(** Pretty-prints a Src_lang. *)

open Common
open Src_lang

(** Converts an expression to a string. *)
let rec string_of_expr : expr -> string = function
  | Array arr ->
    let vals = List.map string_of_expr arr
    in "[" ^ String.concat ", " vals ^ "]"
  | BinOp (op, l, r) ->
    String.concat ""
      [ "("
      ; string_of_expr l
      ; " "
      ; string_of_binop op
      ; " "
      ; string_of_expr r
      ; ")"
      ]
  | Call (name, args) ->
    String.concat ""
      [ name
      ; "("
      ; String.concat ", " (List.map string_of_expr args)
      ; ")"
      ]
  | Fold (func, arr) ->
    String.concat ""
      [ "fold("
      ; func
      ; ", "
      ; string_of_expr arr
      ; ")"
      ]
  | If (c, t, e) ->
    String.concat ""
      [ "if "
      ; string_of_expr c
      ; " then "
      ; string_of_expr t
      ; " else "
      ; string_of_expr e
      ]
  | Int i -> string_of_int i
  | Let (k, v, b) ->
    String.concat ""
      [ "let "
      ; k
      ; " = "
      ; string_of_expr v
      ; " in "
      ; string_of_expr b
      ]
  | Map (func, arr) ->
    String.concat ""
      [ "map("
      ; func
      ; ", "
      ; string_of_expr arr
      ; ")"
      ]
  | MapSeq(func, arr) ->
    String.concat ""
      [ "mapseq("
      ; func
      ; ", "
      ; string_of_expr arr
      ; ")"
      ]
  | Var name -> name

(** Converts a function to a string. *)
and string_of_func (func: func) : string =
  let (name, args, body) = func
  and arg_helper (name, ty) = name ^ " : " ^ string_of_type ty
  in let args = String.concat ", " (List.map arg_helper args)
  in String.concat ""
    [ "function "
    ; name
    ; "("
    ; args
    ; ") =\n\t"
    ; string_of_expr body
    ]

(** Converts a program to a string. *)
and string_of_program (program: program) : string =
  let (funcs, init) = program
  in let funcs = List.map string_of_func funcs
  and init = string_of_expr init
  in String.concat ";\n" (funcs @ [init])

(** Converts a type to a string. *)
and string_of_type : ty -> string = function
  | ArrayType -> "int[]"
  | BoolType -> "bool"
  | FuncType (params, return) ->
    let params = String.concat ", " (List.map string_of_type params)
    and return = string_of_type return
    in "(" ^ params ^ ") -> " ^ return
  | IntType -> "int"
