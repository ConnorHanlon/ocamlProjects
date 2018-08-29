(** Type-checking for Src_lang. *)

exception NotAFunction of string
exception TypeError of Src_lang.expr * Src_lang.ty * Src_lang.ty
exception UndefinedFunction of string
exception UndefinedVar of string

(** Asserts that an expression has the given type. *)
val type_assert : Src_lang.ty Common.Env.t -> Src_lang.ty -> Src_lang.expr -> unit

(** Type-checks an expression. *)
val type_of : Src_lang.ty Common.Env.t -> Src_lang.expr -> Src_lang.ty

(** Type-checks a function. *)
val type_of_func : Src_lang.ty Common.Env.t -> Src_lang.func -> Src_lang.ty
