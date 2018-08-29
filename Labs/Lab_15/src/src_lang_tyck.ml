(** Type-checking for Src_lang. *)

open Common
open Src_lang

exception NotAFunction of string
exception TypeError of expr * ty * ty
exception UndefinedFunction of string
exception UndefinedVar of string

(** Asserts that an expression has the given type. *)
let rec type_assert (env: ty Env.t) (wanted_type: ty) (expr: expr) =
  let got_type = type_of env expr
  in if got_type <> wanted_type
  then raise (TypeError (expr, got_type, wanted_type))

(** Type-checks an expression. *)
and type_of (env: ty Env.t) (expr: expr) : ty =
  match expr with
  | Array arr ->
    ignore (List.map (type_assert env IntType) arr);
    ArrayType
  | BinOp (o, l, r) ->
    type_assert env IntType l;
    type_assert env IntType r;
    (match o with
    | LT -> BoolType
    | _ -> IntType)
  | Call (name, args) ->
    let arg_types = List.map (type_of env) args
    in (match Env.get env name with
        | Some (FuncType (params, return)) ->
          if arg_types <> params
          then raise (TypeError (Var name, FuncType (params, return), return))
          else return
        | Some _ -> raise (NotAFunction name)
        | None -> raise (UndefinedFunction name))
  | Fold (func, arr) ->
    type_assert env (FuncType ([IntType; IntType], IntType)) (Var func);
    type_assert env ArrayType arr;
    IntType
  | If (c, t, e) ->
    type_assert env BoolType c;
    let tt = type_of env t
    and et = type_of env e
    in
    if tt <> et then
        raise (TypeError (e, et, tt))
    else
        tt
  | Int _ -> IntType
  | Let (k, v, b) ->
    let v_type = type_of env v
    in let env' = Env.put env k v_type
    in type_of env' b
  | Map (func, arr) ->
    type_assert env (FuncType ([IntType], IntType)) (Var func);
    type_of env arr
  | MapSeq (func, arr) ->
    type_assert env (FuncType ([IntType], IntType)) (Var func);
    type_of env arr
  | Var name ->
    (match Env.get env name with
     | Some typ -> typ
     | None -> raise (UndefinedVar name))

(** Type-checks a function. *)
and type_of_func (env: ty Env.t) ((name, params, body): func) : ty =
  let param_types = List.map snd params
  in let return_type_hack = FuncType (List.map snd params, IntType)
  in let env' = (name, return_type_hack) :: params @ env
  in let return_type = type_of env' body
  in FuncType (param_types, return_type)
