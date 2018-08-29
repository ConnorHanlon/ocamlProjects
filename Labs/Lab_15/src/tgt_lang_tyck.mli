(** Type-checking for Tgt_lang. *)

exception CallTypeError of Tgt_lang.expr * Tgt_lang.ty list * Tgt_lang.ty list
exception ExprTypeError of Tgt_lang.expr * Tgt_lang.ty * Tgt_lang.ty
exception StmtTypeError of Tgt_lang.stmt * Tgt_lang.expr * Tgt_lang.ty * Tgt_lang.ty
exception InitReturn of Tgt_lang.ty
exception NotAFunction of Tgt_lang.expr * string
exception UndefinedFunc of Tgt_lang.expr * string
exception UndefinedVar of string

(** Asserts that the type of an expression is the given type in a given
 * environment, raising an exception if it is not. *)
val type_assert : Tgt_lang.ty Common.Env.t -> Tgt_lang.ty -> Tgt_lang.expr -> unit

(** Type-checks a program. *)
val type_check_program : Tgt_lang.ty Common.Env.t -> Tgt_lang.program -> unit

(** Type-checks a statement. *)
val type_check_stmt : Tgt_lang.ty Common.Env.t -> Tgt_lang.stmt -> (string * Tgt_lang.ty) option

(** Type-checks a block expression. *)
val type_of_block : Tgt_lang.ty Common.Env.t -> Tgt_lang.stmt list -> Tgt_lang.expr -> Tgt_lang.ty

(** Type-checks an expression. *)
val type_of_expr : Tgt_lang.ty Common.Env.t -> Tgt_lang.expr -> Tgt_lang.ty

(** Type-checks a function. *)
val type_of_func : Tgt_lang.ty Common.Env.t -> Tgt_lang.func -> Tgt_lang.ty
