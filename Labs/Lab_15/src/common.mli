(** Parts used in common between Src_lang and Tgt_lang. *)

(** A binary operation. *)
type binop = Add | Sub | Mul | Div | Mod | LT

(** Converts a binary operation to a string. *)
val string_of_binop : binop -> string

module Env : sig
  (** The type of an environment. *)
  type 'a t = (string * 'a) list

  (** An empty environment. *)
  val empty : 'a t

  (** Tries to get a value from an environment. *)
  val get : 'a t -> string -> 'a option

  (** Puts a value into an environment. *)
  val put : 'a t -> string -> 'a -> 'a t

  (** Joins two environments. *)
  val join : 'a t -> 'a t -> 'a t
end
