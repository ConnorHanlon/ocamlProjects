(** Translates an entire Src_lang program to a Tgt_lang program. *)
val translate : Src_lang.program -> Tgt_lang.program

(** Translates a Src_lang expression in a given environment into a Tgt_lang
 * expression. *)
val translate_expr : Src_lang.ty Common.Env.t -> Src_lang.expr -> Tgt_lang.expr

(** Translates a function definition. *)
val translate_func : Src_lang.ty Common.Env.t -> Src_lang.func -> Tgt_lang.func

(** Translates zeveral functions, adding them to the environment as they are
 * translated. *)
val translate_funcs : Src_lang.ty Common.Env.t -> Src_lang.func list -> Src_lang.ty Common.Env.t * Tgt_lang.func list

(** Translates a type between Src_lang and Tgt_lang. *)
val translate_type : Src_lang.ty -> Tgt_lang.ty

(** Translates a fold. *)
val translate_fold : Src_lang.ty Common.Env.t -> string -> Src_lang.expr -> Tgt_lang.expr

(** Translates a map. *)
val translate_map : Src_lang.ty Common.Env.t -> string -> Src_lang.expr -> Tgt_lang.expr

(** Translates a non-parallel map. *)
val translate_mapseq : Src_lang.ty Common.Env.t -> string -> Src_lang.expr -> Tgt_lang.expr
