let all_odds ls =
    List.filter (fun x -> if x mod 2 = 1 then true else false)ls

let decrement_all ls =
    List.map (fun x -> x-1) ls

let min_fold ls =
    List.fold_left (fun a b -> min a b) (List.hd ls) ls

let sum_prod ls =
    (List.fold_left(fun a b -> a+b) 0 ls, List.fold_left(fun b c -> b*c) 1 ls)
  
let partition_left f lst =
  let f (pass,fail) elem = if f elem
    then (elem::pass, fail)
    else (pass, elem::fail) 
    in
    let
    ls1 = List.fold_left (fun a b -> b::a) [] lst (*had to reverse list in order to work*)
  in 
  List.fold_left(f) ([], [])  ls1

(*it was on your github lol*)
let partition_right f lst =
  let f elem (pass, fail) = if f elem
    then (elem::pass, fail)
    else (pass, elem::fail)
  in 
  List.fold_right(f) lst ([], []) 

(*Used fold right because with fold left, for some reason, I couldn't use the cons to create the list for fold left. Fold right allowed me to do this*)
  let map_as_fold func ls =
    List.fold_right (fun x y -> (func x)::y) ls []