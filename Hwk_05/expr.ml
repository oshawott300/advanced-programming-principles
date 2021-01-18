(* Hwk 05.  Extend the construts below as specified.
 *)

type value 
  = Int of int
  | Bool of bool
  | Closure of string * expr * value_environment
  | Ref of value ref

and value_environment = (string * value) list
                               
and expr 
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr 

  | Lt  of expr * expr
  | Eq  of expr * expr
  | And of expr * expr
  | Not of expr

  | Let of string * typ * expr * expr
  | Id  of string

  | App of expr * expr
  | Lam of string * typ * expr

  | LetRec of string * typ * expr * expr
  | If of expr * expr * expr

and typ = Int_type 
        | Bool_type
        | Func_type of typ * typ

type type_environment = (string * typ option) list 

let type_convert (t: typ) : string =
    match t with 
    | Int_type -> "int"
    | Bool_type -> "bool"
    | Func_type(t1, t2) -> if t1 = Int_type && t2 = Int_type then "int -> int" 
    else if t1 = Int_type && t2 = Bool_type then "int -> bool"
    else if t1 = Bool_type && t2 = Bool_type then "bool -> bool"
    else  "bool -> int"
(* Part 1. Complete unparse *)
let rec unparse (e: expr) : string =
  match e with
  | Val (Int i) -> string_of_int i
  | Val (Bool b) -> string_of_bool b
  | Val(Closure (s, e, env)) -> "<fun>"
  | Val(Ref(_)) -> "reference"
  | Add (x1, x2) -> "(" ^ unparse x1 ^ " + " ^ unparse x2 ^ ")"
  | Sub (x1, x2) -> "(" ^ unparse x1 ^ " - " ^ unparse x2 ^ ")"
  | Mul (x1, x2) -> "(" ^ unparse x1 ^ " * " ^ unparse x2 ^ ")"
  | Div (x1, x2) -> "(" ^ unparse x1 ^ " / " ^ unparse x2 ^ ")"
  | Lt (x1, x2) -> "(" ^ unparse x1 ^ " < " ^ unparse x2 ^ ")"
  | Eq (x1, x2) -> "(" ^ unparse x1 ^ " = " ^ unparse x2 ^ ")"
  | And (x1, x2) -> "(" ^ unparse x1 ^ " && " ^ unparse x2 ^ ")"
  | Not (x1) -> "not (" ^ unparse x1 ^ ")"
  | Let (v,typ,x1,x2) -> "(let " ^ v ^ " : " ^ type_convert(typ) ^ " = " ^ unparse x1 ^ " in " ^ unparse x2 ^ ")"
  | Id (v) -> v
  | App (x1, x2) -> "(" ^ unparse x1 ^ " " ^ unparse x2 ^ ")"
  | Lam (v, typ, x1) -> "(fun (" ^ v ^ ": " ^ type_convert(typ) ^ ") -> " ^ unparse x1 ^ ")"
  | LetRec (v, typ, x1, x2) -> "(let rec " ^ v ^ " : " ^ type_convert(typ) ^ " = " ^ unparse x1 ^ " in " ^ unparse x2 ^ ")"
  | If (x1,x2,x3) -> "(if " ^ unparse x1 ^ " then " ^ unparse x2 ^ " else " ^ unparse x3 ^ ")"

let equal_check v = fun a -> v<>a
let append func x1 x2 = func x1 @ func x2
let append_if func x1 x2 x3 = func x1 @ func x2 @ func x3
(* Part 2. Complete freevars *)       
let rec freevars (e: expr) : string list =
  match e with
  | Val _ -> [] | Add (x1,x2) | Sub (x1,x2) | Mul (x1,x2) | Div (x1,x2) | Lt (x1,x2) | Eq (x1,x2)
  | And (x1,x2) -> append freevars x1 x2
  | Not(x1) -> freevars x1
  | Id v -> [v]
  | App (x1, x2) -> append freevars x1 x2
  | Lam (v, typ, x1) -> List.filter(equal_check v) (freevars x1)
  | Let (v,typ,x1,x2) -> freevars x1 @ List.filter (equal_check v) (freevars x2)
  | LetRec (v, typ, x1, x2) -> List.filter(equal_check v)(freevars x1) @ List.filter(equal_check v)(freevars x2)
  | If(x1,x2,x3) -> append_if freevars x1 x2 x3


       
(* Part 3. Type checking *)           
type result = OK of typ
            | Errs of (expr * string) list
  
(*All helper functions and a lot of the type matches in type_check used from let_int_bool.ml in public repo*)
let expect_Int (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK Int_type -> []
  | OK Bool_type ->  [(e, "expected Int type") ]
  | Errs errs -> errs
  
let expect_Bool (r: result) (e: expr) : (expr * string) list =
  match r with
  | OK Bool_type -> []
  | OK Int_type ->  [(e, "expected Bool type") ]
  | Errs errs -> errs

let rec lookup_typ (x: string)  (t_env: type_environment) : result =
  match t_env with
  | [] -> Errs ( [(Id x, "identifier not bound")] )
  | (y, Some ty)::ys -> if x = y then OK ty else lookup_typ x ys

let rec type_check (e:expr) (env: type_environment) : result =
  match e with
  | Val (Int _) -> OK Int_type
  | Val (Bool _) -> OK Bool_type
  | Val(Closure _) -> failwith "Don't check for closures"
  | Val (Ref _) -> failwith "Don't check for refs"
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) ->
     ( match type_check e1 env, type_check e2 env with
       | OK Int_type, OK Int_type -> OK Int_type
       | r1, r2 -> Errs (expect_Int r1 e1 @ expect_Int r2 e2)
     )
  | Lt (e1, e2) ->
     ( match type_check e1 env, type_check e2 env with
       | OK Int_type, OK Int_type -> OK Bool_type
       | r1, r2 -> Errs (expect_Int r1 e1 @ expect_Int r2 e2)
     )
  | And (e1, e2) ->
     ( match type_check e1 env, type_check e2 env with
       | OK Bool_type, OK Bool_type -> OK Bool_type
       | r1, r2 -> Errs (expect_Bool r1 e1 @ expect_Bool r2 e2)
     )
  |Eq (e1,e2) -> 
    ( match type_check e1 env, type_check e2 env with
      | OK Int_type, OK Int_type -> OK Bool_type
       | r1, r2 -> Errs (expect_Int r1 e1 @ expect_Int r2 e2)
     )
  |Not e1 -> 
    ( match type_check e1 env with
      | OK Bool_type -> OK Bool_type
      | r1 -> Errs (expect_Bool r1 e1) 
    )
  | Id x -> lookup_typ x env
  | Lam (s, t, e) -> 
    (match type_check e ((s, Some t)::env) with 
      | OK x -> OK (Func_type(t, x))
      | r1 -> r1
    )
    
  | App (e1,e2) -> 
    ( match type_check e1 env, type_check e2 env with
      | OK Func_type(t1,t2), OK typ -> if typ = t1 then OK t2 else Errs ([e, "type mismatch"])
      | OK _ , typ_ -> Errs ((e1, "expression 1 doesn't pass"):: (match typ_ with 
      | OK _ -> []
      | Errs errs -> errs))
      | Errs errs , typ2 -> Errs (errs @ (match typ2 with 
      | OK _ -> []
      | Errs errs -> errs))
    )
  | If (e1,e2,e3) ->
    (match type_check e1 env, type_check e2 env, type_check e3 env with
      | OK Bool_type, OK Int_type, OK Int_type -> OK Int_type
      | OK Bool_type, OK Bool_type, OK Bool_type -> OK Bool_type
      | r1, r2, r3 -> Errs([e1, "type mismatch"])
    )
  |LetRec (s,t,e1,e2) ->
     (match type_check e1 ((s, Some t)::env) with
     | Errs errs -> Errs errs
     | OK ty -> if t = ty then type_check e2 ( (s,Some ty) :: env ) else Errs ([(e1, "type mismatch")]))
          
  | Let (x, typ, e1, e2) ->
     (match type_check e1 env with
     | Errs errs -> Errs errs
     | OK ty -> if typ = ty then type_check e2 ( (x,Some ty) :: env ) else Errs ([(e1, "type mismatch")]))

 
       

let check e = type_check e []


(* Part 4. Evaluation *)

let rec eval (env: value_environment) (e:expr) : value =
  match e with
  | Val v -> v

  | Add (e1, e2) ->
     ( match eval env e1, eval env e2 with
       | Int v1, Int v2 -> Int (v1 + v2)
       | _ -> raise (Failure "incompatible types, Add")
     )

  | Let (n, t, dexpr, body) ->
      let v = eval env dexpr in
      eval ( (n,v)::env ) body

  | _ -> failwith "complete this function"


let evaluate e = eval [] e


(* some sample expressions *)

let e1 = Add (Val (Int 3), Val (Int 5))
let e2 = Add (Val (Int 3), Val (Bool true))
let e3 = Mul (Val (Bool true), Val (Int 5))
let e4 = Add (e2, e3)

let e5 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
              Add (Id "x", Val (Int 5))
           )
       
let e6 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
              Lt (Id "x", Val (Int 5))
           )
       
(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e7 = Let ("x", Bool_type,
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "x",
                   Let ("x", Int_type,
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )


(* ``let x = 3 < 5 in y && let x = 1 + 2 in x = 3 *)
let e8 = Let ("x", Bool_type,
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "y",
                   Let ("x", Int_type,
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )

let err_1 = Let ("x", Int_type, Add (Val (Int 3), Val (Int 4)),
                 And (Id "x", Val (Bool true))
              )

let err_2 = Let ("x", Int_type, Add (Id "x", Val (Int 4)),
                 And (Id "y", Val (Bool true))
              )

let inc_use = Let ("inc", Func_type (Int_type, Int_type), 
                   Lam ("n", Int_type, Add (Id "n", Val (Int 1))),
                   App (Id "inc", Val (Int 3))
                )

let sumToN : expr =
    LetRec ("sumToN", Func_type (Int_type, Int_type),
            Lam ("n", Int_type,
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

let sumTo3 = App (sumToN, Val (Int 4))

let inc = Lam ("n", Int_type, Add(Id "n", Val (Int 1)))

let add = Lam ("x", Int_type,
               Lam ("y", Int_type, Add (Id "x", Id "y"))
              )