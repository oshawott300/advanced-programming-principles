type rat = int * int

type op = Add | Sub | Mul | Div

type expr
  = Rat of rat
  | BinOp of expr * op * expr 

let rat_add (n1,d1) (n2,d2) = (n1 * d2 + n2 * d1, d1 * d2)

let rec eval (e:expr) : rat =
  match e with
  | Int i -> i
  | Add (e1, e2) -> eval e1 + eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Div (e1, e2) -> eval e1 / eval e2
  | BinOp (e1, Add, e2) -> rat_add (eval e1) (eval e2)
  | BinOp (e1, Sub, e2) -> rat_add (eval e1) (eval e2)   
  | BinOp (e1, Mul, e2) -> rat_add (eval e1) (eval e2)   
  | BinOp (e1, Div, e2) -> rat_add (eval e1) (eval e2)      

let gcd a' b' =
  let a = if a' < 0 then -a' else a' in
  let b = if b' < 0 then -b' else b' in
  let rec gcd' a b =
    if a = 1 || b = 1 then 1
    else
    if a = b
    then a
    else if a < b
    then gcd' a (b-a)
    else gcd' (a-b) b
  in gcd' a b
   
let rat_simplify (n,d) = 
  if n = 0 then (0,1) else
  let gcd_of_n_d = gcd n d 
  in  (n / gcd_of_n_d, d / gcd_of_n_d)

type evalError = DivByZero | FacOfNegativeNum | FacOfNonWholeNum

exception EvalError of evalError

let show_evalError : evalError -> string = function
  | DivByZero -> "Division by zero"
  | FacOfNegativeNum -> "Factorial of negative number"
  | FacOfNonWholeNum -> "Factorial of non-whole number"

let rec show (e: expr) : string =
  let show_op = function
    | Add -> " + "
    | Sub -> " - "
    | Mul -> " * "
    | Div -> " / "
  in
  match e with
    | BinOp (e1, op, e2) -> "(" ^ show e1 ^ show_op op ^ show e2 ^ ")"
    | Rat (n,d) -> 
       if d = 1 then string_of_int n
       else "(" ^ string_of_int n ^ "/" ^ string_of_int d ^ ")"

exception FoundExpr of expr

let find_expr (rl:rat list) : expr option = 
  let rec build_expr (ce:expr) (rest: rat list) : unit =
    print_endline ("Trying " ^ show ce) ;
    let ce_val : rat option = 
      try Some (rat_simplify (eval ce)) with
      | EvalError er -> print_endline (show_evalError er); None
    in
    match ce_val with
    | None -> ()
    | Some v ->
       if v = (24 ,1) && rest = []
       then
         raise (FoundExpr ce)
       else
         let revlist = List.rev(rl)
         in
         match revlist with
         | x::rest => build_expr x rest
         
