open Common

(** Translates an entire Src_lang program to a Tgt_lang program. *)
let rec translate (src: Src_lang.program) : Tgt_lang.program =
  let (funcs, init) = src
  in let (env, funcs) = translate_funcs Env.empty funcs
  in let init = translate_expr env init
  in (funcs, init)

(** Translates a Src_lang expression in a given environment into a Tgt_lang
 * expression. *)
and translate_expr (env: Src_lang.ty Env.t) (expr: Src_lang.expr) : Tgt_lang.expr =
  match expr with
  | Src_lang.Array arr -> Tgt_lang.Array (List.map (translate_expr env) arr)
  | Src_lang.BinOp (op, l, r) ->
      Tgt_lang.BinOp
        ( op
        , translate_expr env l
        , translate_expr env r
        )
  | Src_lang.Call (name, args) -> Tgt_lang.Call (name, List.map (translate_expr env) args)
  | Src_lang.Fold (func, arr) -> translate_fold env func arr
  | Src_lang.If (c, t, e) ->
    let c' = translate_expr env c
    and t' = translate_expr env t
    and e' = translate_expr env e
    in Tgt_lang.IfExpr (c', t', e')
  | Src_lang.Int i -> Tgt_lang.Int i
  | Src_lang.Let (name, value, body) ->
    let ty = Src_lang_tyck.type_of env value
    in let decl = Tgt_lang.Decl (name, translate_type ty, translate_expr env value)
    and env' = Env.put env name ty
    in Tgt_lang.Block ([decl], translate_expr env' body)
  | Src_lang.Map (func, arr) -> translate_map env func arr
  | Src_lang.MapSeq (func, arr) -> translate_mapseq env func arr
  | Src_lang.Var name -> Tgt_lang.Var name

(** Translates a function definition. *)
and translate_func (env: Src_lang.ty Env.t) (func: Src_lang.func) : Tgt_lang.func =
  let (name, params, body) = func
  in let translate_env (name, ty) = (name, translate_type ty)
  in let return_type_hack = Src_lang.FuncType (List.map snd params, Src_lang.IntType)
  in let env' = ((name, return_type_hack) :: params @ env)
  in let env'' = List.map translate_env env'
  and params' = List.map translate_env params
  and body' = translate_expr env' body
  in let ty = Tgt_lang_tyck.type_of_expr env'' body'
  in (name, params', ty, body')

(** Translates zeveral functions, adding them to the environment as they are
 * translated. *)
and translate_funcs (env: Src_lang.ty Env.t) (funcs: Src_lang.func list) : Src_lang.ty Env.t * Tgt_lang.func list =
  match funcs with
  | []   -> (Env.empty, [])
  | h::t ->
    let func = translate_func env h
    and ty = Src_lang_tyck.type_of_func env h
    and (name, _, _) = h
    in let (rest_env, rest_funcs) = translate_funcs (Env.put env name ty) t
    in (Env.put rest_env name ty, func :: rest_funcs)

(** Translates a type between Src_lang and Tgt_lang. *)
and translate_type : Src_lang.ty -> Tgt_lang.ty = function
  | Src_lang.ArrayType -> Tgt_lang.ArrayType
  | Src_lang.BoolType -> Tgt_lang.BoolType
  | Src_lang.FuncType (params, return) ->
    let params' = List.map translate_type params
    and return' = translate_type return
    in Tgt_lang.FuncType (params', return')
  | Src_lang.IntType -> Tgt_lang.IntType

(** Translates a fold. *)
and translate_fold (env: Src_lang.ty Env.t) (func: string) (arr: Src_lang.expr) : Tgt_lang.expr =
  let arr' = translate_expr env arr
  and arr_type = Src_lang_tyck.type_of env arr
  and func' = translate_expr env (Src_lang.Var func)
  and func_type = Src_lang_tyck.type_of env (Src_lang.Var func)
  in if func_type <> Src_lang.FuncType ([Src_lang.IntType; Src_lang.IntType], Src_lang.IntType)
  then raise (Tgt_lang_tyck.ExprTypeError
                ( func'
                , translate_type func_type
                , Tgt_lang.FuncType ([Tgt_lang.IntType; Tgt_lang.IntType], Tgt_lang.IntType)))
  else if arr_type <> Src_lang.ArrayType
  then raise (Tgt_lang_tyck.ExprTypeError (arr', translate_type arr_type, Tgt_lang.ArrayType))
  (* This should build the following:
   *     
   * int fold_helper(array _fold_arr, int start, int end) {
   *     int out = 0;
   *     if((end - start) == 1) {
   *         out = _fold_arr[start];
   *     } else {
   *         int split = (start + ((end - start) / 2));
   *         int leftVal = spawn fold_helper(_fold_arr, start, split);
   *         int rightVal = spawn fold_helper(_fold_arr, split, end);
   *         sync;
   *         out = add(left, right);
   *     }
   * 
   *     return array_get(out, 0);
   * }
   * array _fold_arr = arr;
   * return fold_helper(_fold_arr, 0, _fold_arr.size);
  *)
  else failwith "TODO translate_fold"

(** Translates a map. *)
and translate_map (env: Src_lang.ty Env.t) (func: string) (arr: Src_lang.expr) : Tgt_lang.expr =
  let arr' = translate_expr env arr
  and arr_type = Src_lang_tyck.type_of env arr
  and func' = translate_expr env (Src_lang.Var func)
  and func_type = Src_lang_tyck.type_of env (Src_lang.Var func)
  in if func_type <> Src_lang.FuncType ([Src_lang.IntType], Src_lang.IntType)
  then raise (Tgt_lang_tyck.ExprTypeError
                ( func'
                , translate_type func_type
                , Tgt_lang.FuncType ([Tgt_lang.IntType], Tgt_lang.IntType)))
  else if arr_type <> Src_lang.ArrayType
  then raise (Tgt_lang_tyck.ExprTypeError (arr', translate_type arr_type, Tgt_lang.ArrayType))
  else let array = Tgt_lang.Var "_map_array"
    and index = Tgt_lang.Var "_map_index"
    (* This should be:
     *
     * array _map_array = arr;
     * for(int _map_index = 0; _map_index < _map_array.size; _map_index++) {
     *     spawn _map_array[_map_index] = f(_map_array[_map_index]);
     * }
     * sync;
     *
     * Note the similarity to the code in translate_map.
    *)
    in failwith "TODO translate_map"

(** Translates a non-parallel map. *)
and translate_mapseq (env: Src_lang.ty Env.t) (func: string) (arr: Src_lang.expr) : Tgt_lang.expr =
  let arr' = translate_expr env arr
  and arr_type = Src_lang_tyck.type_of env arr
  and func' = translate_expr env (Src_lang.Var func)
  and func_type = Src_lang_tyck.type_of env (Src_lang.Var func)
  in if func_type <> Src_lang.FuncType ([Src_lang.IntType], Src_lang.IntType)
  then raise (Tgt_lang_tyck.ExprTypeError
                ( func'
                , translate_type func_type
                , Tgt_lang.FuncType ([Tgt_lang.IntType], Tgt_lang.IntType)))
  else if arr_type <> Src_lang.ArrayType
  then raise (Tgt_lang_tyck.ExprTypeError (arr', translate_type arr_type, Tgt_lang.ArrayType))
  else let array = Tgt_lang.Var "_map_array"
    and index = Tgt_lang.Var "_map_index"
    (* This is roughly:
     *
     * array _map_array = arr;
     * for(int _map_index = 0; _map_index < _map_array.size; _map_index++) {
     *     _map_array[_map_index] = f(_map_array[_map_index]);
     * }
    *)
    in Tgt_lang.Block (
      [ Tgt_lang.Decl ("_map_array", Tgt_lang.ArrayType, arr')
      ; Tgt_lang.For ("_map_index", Tgt_lang.Int 0, Tgt_lang.ArraySize array,
                      [ Tgt_lang.ArraySet
                          ( array
                          , index
                          , Tgt_lang.Call
                              ( func
                              , [ Tgt_lang.ArrayGet (array, index)
                                ]
                              )
                          )
                      ])
      ], Tgt_lang.Var "_map_array")
