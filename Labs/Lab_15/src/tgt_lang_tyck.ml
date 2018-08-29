(** Type-checking for Tgt_lang. *)

open Common
open Tgt_lang

exception CallTypeError of expr * ty list * ty list
exception ExprTypeError of expr * ty * ty
exception StmtTypeError of stmt * expr * ty * ty
exception InitReturn of ty
exception NotAFunction of expr * string
exception UndefinedFunc of expr * string
exception UndefinedVar of string

let rec find (pred: 'a -> bool) : 'a list -> 'a option = function
  | []   -> None
  | h::t -> if pred h
    then Some h
    else find pred t

(** Asserts that the type of an expression is the given type in a given
 * environment, raising an exception if it is not. *)
let rec type_assert (env: ty Env.t) (wanted_type: ty) (expr: expr) : unit =
  let got_type = type_of_expr env expr
  in if got_type <> wanted_type
  then raise (ExprTypeError (expr, got_type, wanted_type))

(** Type-checks a program. *)
and type_check_program (env: ty Env.t) (program: program) : unit =
  let (funcs, init) = program
  in match funcs with
  | [] ->
    let init_type = type_of_expr env init
    in (match init_type with
        | FuncType _ -> raise (InitReturn init_type)
        | _ -> ())
  | h::t ->
    let (name, _, _, _) = h
    and ty = type_of_func env h
    in let env' = Env.put env name ty
    in type_check_program env' (t, init)

(** Type-checks a statement. *)
and type_check_stmt (env: ty Env.t) (stmt: stmt) : (string * ty) option =
  match stmt with
  | ArraySet (arr, index, value) ->
    type_assert env ArrayType arr;
    type_assert env IntType index;
    type_assert env IntType value;
    None
  | Decl (name, ty, expr) ->
    type_assert env ty expr;
    Some (name, type_of_expr env expr)
  | For (idx_name, start, end_, body) ->
    let env' = Env.put env idx_name IntType
    in ignore (List.map (type_check_stmt env') body);
    None
  | FuncDecl func ->
    let (name, params, return, _) = func
    in let ty = FuncType (List.map snd params, return)
    in let env' = Env.put env name ty
    in Some (name, type_of_func env' func)
  | If (c, t, e) ->
    type_assert env BoolType c;
    None
  | Set (name, expr) ->
    type_assert env IntType (Var name);
    type_assert env IntType expr;
    None
  | Sleep _ | Sync -> None

(** Type-checks a block expression. *)
and type_of_block (env: ty Env.t) (stmts: stmt list) (expr: expr) : ty =
  match stmts with
  | []   -> type_of_expr env expr
  | h::t ->
    let env' = match type_check_stmt env h with
      | Some (name, ty) -> Env.put env name ty
      | None              -> env
    in type_of_block env' t expr

and type_of_expr (env: ty Env.t) (expr: expr) : ty =
  match expr with
  | Array arr ->
    ignore (List.map (type_assert env IntType) arr);
    ArrayType
  | ArrayGet (arr, idx) ->
    type_assert env ArrayType arr;
    type_assert env IntType idx;
    IntType
  | ArraySize arr ->
    type_assert env ArrayType arr;
    IntType
  | Bool _ -> BoolType
  | BinOp (o, l, r) ->
    type_assert env IntType l;
    type_assert env IntType r;
    (match o with
    | LT -> BoolType
    | _ -> IntType)
  | Block (stmts, expr) -> type_of_block env stmts expr
  | Call (func, args) ->
    (match Env.get env func with
     | Some (FuncType (params, ret)) ->
       let arg_types = List.map (type_of_expr env) args
       in if arg_types <> params
       then raise (CallTypeError (expr, arg_types, params))
       else ret
     | Some _ -> raise (NotAFunction (expr, func))
     | None -> raise (UndefinedFunc (expr, func)))
  | Equals (l, r) ->
    type_assert env IntType l;
    type_assert env IntType r;
    BoolType
  | IfExpr (c, t, e) ->
    type_assert env BoolType c;
    let tt = type_of_expr env t
    and et = type_of_expr env e
    in
    if tt <> et then
        raise (ExprTypeError (e, et, tt))
    else
        tt
  | Int _ -> IntType
  | Spawn expr -> type_of_expr env expr
  | Var name ->
    (match Env.get env name with
     | Some typ -> typ
     | None     -> raise (UndefinedVar name))

and type_of_func (env: ty Env.t) (func: func) : ty =
  let (name, params, return, body) = func
  in let body_type = type_of_expr ((name, FuncType (List.map snd params, return)) :: params @ env) body
  in if return <> body_type
  then raise (ExprTypeError (body, body_type, return))
  else FuncType (List.map snd params, return)
