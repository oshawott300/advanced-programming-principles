let odd x = 
    if x mod 2 = 1 then true
    else false

let rec euclid a b =
    let c = a-b in
    let d = b-a in 
    if a = b then a
    else if a < b then euclid a d
    else euclid c b

let frac_simplify (x, y) =
    let z = euclid x y in (x/z, y/z)

let rec min_list ls =
    match ls with
    | [] -> 0
    | x::[] -> x
    | x::xs -> 
    let min_elem = min_list xs in
        if x<min_elem then x
        else min_elem

let rec drop elem ls = 
    if elem=0 then ls
    else
        match ls with
        | [] -> []
        | [a] -> []
        | b::rest -> drop (elem-1) rest
