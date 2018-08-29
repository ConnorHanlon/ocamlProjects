(** Pretty-prints a Src_lang. *)

(** Converts an expression to a string. *)
val string_of_expr : Src_lang.expr -> string

(** Converts a function to a string. *)
val string_of_func : Src_lang.func -> string

(** Converts a program to a string. *)
val string_of_program : Src_lang.program -> string

(** Converts a type to a string. *)
val string_of_type : Src_lang.ty -> string
