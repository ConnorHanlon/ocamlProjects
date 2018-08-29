(** Pretty-printing for Tgt_lang. *)

open Common
open Tgt_lang

(** Converts a program to a string. *)
let rec string_of_program (program: program) : string =
  let (funcs, init) = program
  in let funcs = String.concat "\n" (List.map string_of_func funcs)
  and env = List.fold_left (fun env func ->
      let (name, params, return, _) = func
      in let ty = FuncType (List.map snd params, return)
      in Env.put env name ty) Env.empty funcs
  in let init_type = Tgt_lang_tyck.type_of_expr env init
  in let init = string_of_func ("init", [], init_type, init)
  in String.concat ""
    ["#include \"runtime.h\"\n\n"
    ; funcs
    ; "\n"
    ; init
    ; "\n\ndeclare_main_returning_"
    ; string_of_type init_type
    ; "()\n"
    ]

(** Converts an expr to a string. *)
and string_of_expr : expr -> string = function
  | Array arr ->
    String.concat ""
      [ "make_array("
      ; string_of_int (List.length arr)
      ; ", "
      ; String.concat ", " (List.map string_of_expr arr)
      ; ")"
      ]
  | ArrayGet (arr, idx) ->
    String.concat ""
      [ "array_get("
      ; string_of_expr arr
      ; ", "
      ; string_of_expr idx
      ; ")"
      ]
  | ArraySize arr ->
    String.concat ""
      [ string_of_expr arr
      ; ".size"
      ]
  | BinOp (op, l, r) ->
    let inner = String.concat " "
        [ string_of_expr l
        ; string_of_binop op
        ; string_of_expr r
        ]
    in "(" ^ inner ^ ")"
  | Bool true -> "true"
  | Bool false -> "false"
  | Block (stmts, expr) ->
    String.concat ""
      [ "({\n"
      ; String.concat "\n" (List.map string_of_stmt stmts)
      ; "\n"
      ; string_of_expr expr
      ; ";\n})"
      ]
  | Call (func, args) ->
    String.concat ""
      [ func
      ; "("
      ; String.concat ", " (List.map string_of_expr args)
      ; ")"
      ]
  | Equals (a, b) -> string_of_expr a ^ " == " ^ string_of_expr b
  | IfExpr (c, t, e) ->
    String.concat ""
      [ "(("
      ; string_of_expr c
      ; ") ? ("
      ; string_of_expr t
      ; ") : ("
      ; string_of_expr e
      ; "))"
      ]
  | Int i -> string_of_int i
  | Spawn expr -> "spawn " ^ string_of_expr expr
  | Var name -> name

(** Converts a function to a string. *)
and string_of_func (func: func) : string =
  let (name, params, return_type, body) = func
  in let string_of_param (name, ty) = string_of_type_decl name ty
  in let params = List.map string_of_param params
  and func_body = match body with
    | Block (stmts, expr) ->
      let stmts = String.concat "\n" (List.map string_of_stmt stmts)
      and expr = "return " ^ string_of_expr expr ^ ";"
      in stmts ^ "\n" ^ expr
    | _ -> "return " ^ string_of_expr body ^ ";"
  in let func_name = name ^ "(" ^ String.concat ", " params ^ ")"
  in String.concat ""
    [ string_of_type_decl func_name return_type
    ; " {\n"
    ; func_body
    ; "\n}\n"
    ]

(** Converts a statement to a string. *)
and string_of_stmt : stmt -> string = function
  | ArraySet (arr, idx, value) ->
    String.concat ""
      [ string_of_expr arr
      ; ".data["
      ; string_of_expr idx
      ; "] = "
      ; string_of_expr value
      ; ";"
      ]
  | Decl (name, ty, value) ->
    String.concat ""
      [ string_of_type_decl name ty
      ; " = "
      ; string_of_expr value
      ; ";"
      ]
  | For (idx_name, start, end_, body) ->
    let body' = String.concat "\n" (List.map string_of_stmt body)
    in String.concat ""
      [ "for(int "
      ; idx_name
      ; " = "
      ; string_of_expr start
      ; "; "
      ; idx_name
      ; " < "
      ; string_of_expr end_
      ; "; "
      ; idx_name
      ; "++) {\n"
      ; body'
      ; "}\n"
      ]
  | FuncDecl func -> string_of_func func
  | If (c, t, e) ->
    String.concat ""
      [ "if("
      ; string_of_expr c
      ; ") {"
      ; String.concat "\n" (List.map string_of_stmt t)
      ; "} else {"
      ; String.concat "\n" (List.map string_of_stmt e)
      ; "}"
      ]
  | Set (name, expr) -> name ^ " = " ^ string_of_expr expr ^ ";"
  | Sleep time ->
    String.concat ""
      [ "usleep("
      ; string_of_int (truncate (time *. 1_000_000.))
      ; ");"
      ]
  | Sync -> "sync;"

(** Converts a type to a string. *)
and string_of_type : ty -> string = function
  | ArrayType -> "array"
  | BoolType -> "bool"
  | FuncType (params, return) ->
    String.concat ""
      [ string_of_type return
      ; "(*)("
      ; String.concat ", " (List.map string_of_type params)
      ; ")"
      ]
  | IntType -> "int"

(** Converts a type declaration to a string. *)
and string_of_type_decl (name: string) : ty -> string = function
  | ArrayType -> "array " ^ name
  | BoolType -> "bool " ^ name
  | FuncType (params, return) ->
    String.concat ""
      [ string_of_type return
      ; "(*"
      ; name
      ;")("
      ; String.concat ", " (List.map string_of_type params)
      ; ")"
      ]
  | IntType -> "int " ^ name
