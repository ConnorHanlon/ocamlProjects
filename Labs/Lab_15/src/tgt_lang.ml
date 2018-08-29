(** The types associated with the Tgt_lang. *)

(** A type. *)
type ty
(** The type of an int array. *)
  = ArrayType
  (** The type of a boolean. *)
  | BoolType
  (** The type of a function. *)
  | FuncType of ty list * ty
  (** The type of an integer. *)
  | IntType

(** A function.
 *
 * The fields here are, in order:
 * 
 *  - names
 *  - arguments
 *  - return type
 *  - body expression *)
type func = string * (string * ty) list * ty * expr

(** An expression. *)
and expr
(** An array literal. *)
  = Array of expr list
  (** Retrieves a value from an array. *)
  | ArrayGet of expr * expr
  (** Gets the size of an array. *)
  | ArraySize of expr
  (** A boolean literal. *)
  | Bool of bool
  (** A binary operation. *)
  | BinOp of Common.binop * expr * expr
  (** A block of statements to execute, and an expression. The statements run
   * in a new child scope, so any variables declared inside the block will not
   * persist to outside of the block. *)
  | Block of stmt list * expr
  (** A function call. *)
  | Call of string * expr list
  (** A check for equality. *)
  | Equals of expr * expr
  (** A conditional expression. *)
  | IfExpr of expr * expr * expr
  (** An integer literal. *)
  | Int of int
  (** Spawns an expression. *)
  | Spawn of expr
  (** A variable. *)
  | Var of string

(** A statement. *)
and stmt
(** An array assignment. *)
  = ArraySet of expr * expr * expr
  (** A variable declaration. *)
  | Decl of string * ty * expr
  (** A function declaration. *)
  | FuncDecl of func
  (** A for loop. *)
  | For of string * expr * expr * stmt list
  (** A conditional. *)
  | If of expr * stmt list * stmt list
  (** A variable write to an int. *)
  | Set of string * expr
  (** Sleeps for a certain amount of time. *)
  | Sleep of float
  (** Synchronizes, forcing any spawned expressions to complete. *)
  | Sync

(** The type of a complete program. *)
type program = func list * expr

