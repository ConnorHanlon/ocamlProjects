(** Pretty-printing for Tgt_lang. *)

(** Converts a program to a string. *)
val string_of_program : Tgt_lang.program -> string

(** Converts an expr to a string. *)
val string_of_expr : Tgt_lang.expr -> string

(** Converts a function to a string. *)
val string_of_func : Tgt_lang.func -> string

(** Converts a statement to a string. *)
val string_of_stmt : Tgt_lang.stmt -> string

(** Converts a type to a string. *)
val string_of_type : Tgt_lang.ty -> string

(** Converts a type declaration to a string. *)
val string_of_type_decl : string -> Tgt_lang.ty -> string
