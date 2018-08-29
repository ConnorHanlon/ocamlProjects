(* Adding function values, recursive let expressions, lambda
   expressions and function application. 
   This language extends the language in ``expr_let.ml``.
   Eric Van Wyk   
 *)

type expr 
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr 

  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr
  | Not of expr

  | Let of string * expr * expr
  | Id of string

  | App of expr * expr
  | Lambda of string * expr

  | LetRec of string * expr * expr
  | If of expr * expr * expr

and value
  = Int of int
  | Bool of bool
  | Closure of string * expr * environment
  | Rec of value ref

and environment = (string * value) list

let rec unparse (e:expr) : string =
  match e with
  | Add (e1, e2) -> "(" ^ unparse e1 ^ " + " ^ unparse e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ unparse e1 ^ " - " ^ unparse e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ unparse e1 ^ " * " ^ unparse e2 ^ ")"
  | Div (e1, e2) -> "(" ^ unparse e1 ^ " / " ^ unparse e2 ^ ")"
  | Lt (e1, e2) -> "(" ^ unparse e1 ^ " < " ^ unparse e2 ^ ")"
  | Eq (e1, e2) -> "(" ^ unparse e1 ^ " = " ^ unparse e2 ^ ")"
  | And (e1, e2) -> "(" ^ unparse e1 ^ " && " ^ unparse e2 ^ ")"
  | Not e1 -> "!" ^ unparse e1
  | Let (s, e1, e2) -> "(let " ^ s ^ " = "
     ^ unparse e1 ^ " in " ^ unparse e2 ^ ")"
  | Id s -> s
  | App (e1, e2) -> "(" ^ unparse e1 ^ " " ^ unparse e2 ^ ")"
  | Lambda (s, e1) -> "(fun " ^ s ^ " -> " ^  unparse e1 ^ ")"
  | LetRec (s, e1, e2) -> "(fun " ^ s ^ " -> ("
     ^ unparse e1 ^ ", " ^ unparse e2 ^ ")"
  | If (e1, e2, z) -> "if " ^ unparse e1 ^ " then "
     ^ unparse e2 ^ " else " ^ unparse z
  | Val v -> match v with
    |Int i -> string_of_int i 
    |Bool b -> string_of_bool b
    | _ -> raise ( Failure "Will only serialize integer and Boolean values.")


  
let rec serialize (e:expr): string =
  match e with
  | Add (e1, e2) -> "Add ("  ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Sub (e1, e2) -> "Sub ("  ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Mul (e1, e2) -> "Mul ("  ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Div (e1, e2) -> "Div ("  ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Lt (e1, e2) -> "Lt ("  ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Eq (e1, e2) -> "Eq ("  ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | And (e1, e2)-> "And ("  ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Not e1 -> "Not " ^ serialize e1
  | Let (s, e1, e2) -> "Let " ^ "(\"" ^ s ^ "\", "
     ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Id s -> "Id " ^ "\"" ^ s ^ "\"" 
  | App (e1, e2) -> "App (" ^ serialize e1 ^ ", " ^ serialize e2 ^ ")"
  | Lambda (s, e) -> "Lambda " ^ "(\"" ^ s ^ "\", " ^ serialize e ^ ")"
  | LetRec (s, e1, e2) -> "LetRec " ^ "(\"" ^ s ^ "\", "
     ^ serialize e1 ^ ", " ^  serialize e2 ^ ")"
  | If (e1, e2, e3) -> "If ("  ^ serialize e1 ^ ", "
     ^ serialize e2 ^ ", " ^ serialize e3 ^ ")"
  | Val v -> match v with
    |Int i -> "Val (Int " ^ string_of_int i ^ ")"
    |Bool b -> "Val (Bool " ^ string_of_bool b ^ ")"
    | _ -> raise ( Failure "Will only serialize integer and Boolean values.")
  
let rec lookup (n: string) (env: environment) : value =
  match env with
  | [] -> raise (Failure ("Name \"" ^ n ^ "\" not in scope"))
  | (n',v)::_ when n' = n ->
     (match v with
    |(Int i) -> (Int i)
    |(Bool b) -> (Bool b)
    | Closure (s, e1, e2) -> Closure (s, e1, e2)
    | Rec (r) -> !r
     )
  | _::rest -> lookup n rest

(*fix freevars *)
let rec freevars (e: expr) : string list = 
  match e with
  | Val _ -> []
  | Add (e1, e2) -> freevars e1 @ freevars e2
  | Sub (e1, e2) -> freevars e1 @ freevars e2
  | Mul (e1, e2) -> freevars e1 @ freevars e2
  | Div (e1, e2) -> freevars e1 @ freevars e2
  | Lt (e1, e2) -> freevars e1 @ freevars e2
  | Eq (e1, e2) -> freevars e1 @ freevars e2
  | And (e1, e2) -> freevars e1 @ freevars e2
  | Not e1 -> freevars e1
  | Let (n, dexpr, body) ->
     freevars dexpr @ (List.filter (fun n' -> n <> n') (freevars body))
  | Id n -> [n]
  | App (e1, e2) -> freevars e1 @ freevars e2
  | Lambda (s, e) ->
     [] @ (List.filter (fun s' -> s <> s') (freevars e))
  | LetRec (s, e1, e2) ->
     [] @ (List.filter (fun s' -> s <> s') (freevars e1)) @
       (List.filter (fun s' -> s <> s') (freevars e2))
  | If (e1, e2, e3) -> []

(*Evaluates the first expression, matches for certain cases and
 depending on the match will then evaluate subsequent expressions.  *)
let rec eval (env: environment) (e: expr) : value = 
  match e with
  | Val v -> v
  | Add (e1, e2) ->
     (match eval env e1 with
     | Int i1 ->
	(match eval env e2 with
	| Int i2 -> Int (i1 + i2)
	| _ -> raise (Failure "Incompatible second value for add")
	)
     | _ -> raise (Failure "Incompatible first value for add")
     )
	
  | Sub (e1, e2) ->
     (match eval env e1 with
     | Int i1 ->
	(match eval env e2 with
	| Int i2 -> Int (i1 - i2)
	| _ -> raise (Failure "Incompatible second value for sub")
	)
     | _ -> raise (Failure "Incompatible first value for sub")
     )

  | Mul (e1, e2) ->
     (match eval env e1 with
     | Int i1 ->
	(match eval env e2 with
	| Int i2 -> Int (i1 * i2)
	| _ -> raise (Failure "Incompatible second value for mul")
	)
     | _ -> raise (Failure "Incompatible first value for mul")
     )

  | Div (e1, e2) ->
     (match eval env e1 with
     | Int i1 ->
	(match eval env e2 with
	| Int 0 -> raise (Failure "Cannot divide by zero")
	| Int i2 -> Int (i1 / i2)
	| _ -> raise (Failure "Incompatible second value for div")
	)
     | _ -> raise (Failure "Incompatible first value for div")
     )

  | Lt (e1, e2) ->
     (match eval env e1 with
     | Int i1 ->
	(match eval env e2 with
	| Int i2 -> Bool (i1 < i2)
	| _ -> raise (Failure "Incompatible second value for lt")
	)
     | _ -> raise (Failure "Incompatible first value for lt")
     )
       
  | Eq (e1, e2) ->
     (match eval env e1 with
     | Int i1 ->
	(match eval env e2 with
	| Int i2 -> Bool (i1 = i2)
	| _ -> raise (Failure "Incompatible second value for eq")
	)
     | Bool b1 ->
	(match eval env e2 with
	| Bool b2 -> Bool (b1 = b2)
	| _ -> raise (Failure "Incompatible second value for eq")
	)
     | _ -> raise (Failure "First value expected of type Int or Bool for Eq")
     )
       
  | And (e1, e2) ->
     (match eval env e1 with
     | Bool b1 ->
	(match eval env e2 with
	| Bool b2 -> Bool (b1 && b2)
	| _ -> raise (Failure "Incompatible second value for and")
	)
     | _ -> raise (Failure "Incompatible first value for and")
     )
     
  | Not e1 ->
     (match eval env e1 with
     | Bool b -> Bool (not b)
     | _ -> raise (Failure "Incompatible value for not")
     )
  | Let (n, dexpr, body) -> 
      let v = eval env dexpr in
      eval ((n,v)::env ) body
  | Id n -> lookup n env
  | App (e1, e2) -> 
     (match eval env e1, eval env e2 with 
     | Closure (s, ex, en), Int i ->
	let appd = (s, Int i) :: en
	in
	eval appd  ex
     | Closure (s, ex, en), _ -> Closure (s, ex, en)
     | _, _ -> raise (Failure "Incompatible value for App")
     )
  | Lambda (s, e1) -> Closure (s, e1, env) 
  | LetRec (n, e1, e2) ->
     (match eval env e1 with
     | Closure (s, e3, en) ->
	let placeholdr = ref (Int 999) in
	let c =
	Closure (s, e3, (n, Rec placeholdr)::en)
	in
	let () = placeholdr := c
	in
      	let appd = (n, c)::en in
      	eval appd e2
     | _ -> raise (Failure "Incompatible value for LetRec")
    )
  | If (e1, e2, e3) ->
     (match eval env e1 with
     | Bool b1 ->
	(match b1 with
	| true ->
	  (match eval env e2 with
	  | Int i -> Int i
	  | _ -> raise (Failure "Incompatible expression 2 for if")
	)
	| false ->
	  (match eval env e3 with
	  | Int i -> Int i
	  | _ -> raise (Failure "Incompatible expression 3 for if")
	  )
	)
     | _ -> raise (Failure "Incompatible expression 1 for if")
     )

       
let evaluate e = eval [] e
  

(* Some sample expressions and their values *)
let e1 = Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))
(*let v1 = evaluate e1*)

let e2 = Sub (Val (Int 10), Div (e1, Val (Int 2)))
(*let v2 = evaluate e2*)

let e3 = Eq (e1, e2)
let e4 = Lt (e1, e2)

let e5 = Not e4

(* ``let y = 5 in let x = y + 5 in x + y'' *)
let e6 = Let ("y", 
              Val (Int 5), 
              Let ("x", 
                   Add (Id "y", Val (Int 5)), 
                   Add (Id "x", Id "y")
                  )
             )
(*Passed Tests*)
let () =
  assert (serialize e1 = "Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))");
  assert (serialize e6 = 
            "Let (\"y\", Val (Int 5), Let (\"x\", " ^ 
              "Add (Id \"y\", Val (Int 5)), Add (Id \"x\", Id \"y\")))")


(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e7 = Let ("x",
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "x",
                   Let ("x",
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
)

  
(*Passed Tests*)
  
let () =
  assert (evaluate e1 = Int 7);
  assert (evaluate e2 = Int 7);
  assert (evaluate e3 = Bool true);
  assert (evaluate e4 = Bool false);
  assert (evaluate e5 = Bool true);
  assert (evaluate e6 = Int 15);
  assert (evaluate e7 = Bool true)
  
  
(* increment *)
let inc = Lambda ("n", Add(Id "n", Val (Int 1)))

let add = Lambda ("x",
                  Lambda ("y", Add (Id "x", Id "y"))
                 )
let inc' = App (add, Val (Int 1))

let myt1 = App (add, Val (Int 1))
let myt2 = App ( (App (add, Val (Int 1))), Val (Int 2))

(* The add2 closure *)
let add2app =
  Let ("add2",
       Let ("two", Val (Int 2), Lambda ("x", Add (Id "x", Id "two"))),
       App (Id "add2", Val (Int 4)))

let () =
  assert (evaluate (App (inc, Val (Int 4))) = Int 5);
  assert (evaluate (Add (Val (Int 2), Val (Int 3))) = Int 5);
  assert (evaluate (App (inc', Val (Int 4))) = Int 5);
  assert (evaluate add2app = Int 6)


(* sumToN *)
let sumToN : expr =
    LetRec ("sumToN", 
            Lambda ("n", 
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 0),
                        Add (Id "n", 
                             App (Id "sumToN", 
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
            Id "sumToN"
           )

(* factorial *)
let fact : expr =
    LetRec ("fact", 
            Lambda ("n", 
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 1),
                        Mul (Id "n", 
                             App (Id "fact", 
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
            Id "fact"
    )

let rs0 = (App (sumToN, Val (Int 0)))
let rs1 = (App (sumToN, Val (Int 4)))
let rs2 = (App (sumToN, Val (Int 10)))
let rs3 = (App (sumToN, Val (Int 100)))
let rs4 = (App (fact, Val (Int 0)))
let rs5 = (App (fact, Val (Int 1)))
  
    
  

(* Assert expressions to test our functions. *)
let () =
  assert (evaluate (App (sumToN, Val (Int 4))) = Int 10);
  assert (evaluate (App (sumToN, Val (Int 10))) = Int 55);
  assert (evaluate (App (sumToN, Val (Int 100))) = Int 5050);
  assert (evaluate (App (fact, Val (Int 0))) = Int 1);
  assert (evaluate (App (fact, Val (Int 1))) = Int 1);
  assert (evaluate (App (fact, Val (Int 2))) = Int 2);
  assert (evaluate (App (fact, Val (Int 4))) = Int 24)


(* If utop gets to this point without raising an ``assert`` exception
   then all tests have passed. *)
let () =
  print_endline ("Success! All tests passed.")

  
