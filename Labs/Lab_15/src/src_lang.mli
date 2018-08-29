(** The Src_lang, the language you're compiling from. *)

(** The type of a Src_lang type. *)
type ty
  (** An array of ints. *)
  = ArrayType
  (** A boolean. *)
  | BoolType
  (** A function. *)
  | FuncType of ty list * ty
  (** An integer. *)
  | IntType

(** The type of a function. *)
type func = string * (string * ty) list * expr

(** The type of an expression. *)
and expr
(** An array literal. *)
  = Array of expr list
  (** A binary operator. *)
  | BinOp of Common.binop * expr * expr
  (** A function call. *)
  | Call of string * expr list
  (** A fold. *)
  | Fold of string * expr
  (** A conditional. *)
  | If of expr * expr * expr
  (** An integer literal. *)
  | Int of int
  (** A let binding. *)
  | Let of string * expr * expr
  (** A map. *)
  | Map of string * expr
  (** A non-parallel map. *)
  | MapSeq of string * expr
  (** A variable call. *)
  | Var of string

(** A program. *)
type program = func list * expr
