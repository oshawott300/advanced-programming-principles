(* You may use the definitions in your solutions on the final
   project.  Be sure to properly attribute the author of these
   functions if you use them.
 *)

(*all read file functions and explode and implode functions from read_file.ml by Eric Van Wyk*)

let read_file (filename:string) : string list =
  let rec read_lines channel sofar =
    try 
      let ln = input_line channel
      in read_lines channel (ln :: sofar)
    with
    | End_of_file -> sofar
    | e -> raise e
  in
  try 
    let channel = open_in filename
    in 
    let lines_in_reverse = read_lines channel []
    in List.rev lines_in_reverse
  with
  | e -> raise e

let rec explode : string -> char list = function
  | "" -> []
  | s  -> String.get s 0 :: explode (String.sub s 1 ((String.length s) - 1))

let rec implode : char list -> string = function
  | []    -> ""
  | c::cs -> String.make 1 c ^ implode cs

let d1 = "words-small.txt"
let d2 = "words-google-10000.txt"
let d3 = "words-corncob.txt"

(*drop function and take function from HW2 paradelle by Eric Van Wyk *)
let rec drop n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then drop (n-1) xs else l

let rec take n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then x::take (n-1) xs else []

let explode_file ls = List.map(fun s -> explode s) ls

let it_takes_two (str:string) : (string * string) list =
    let file = read_file str
    in
    let drop_words_file = List.map (fun s -> drop 1 s) (explode_file file)
    in
    let take_words_file = List.map (fun s -> take (List.length(s) - 1) s) drop_words_file
    in
    let modified_words_file = List.map(fun s -> implode s) take_words_file
    in 
    let combined_file = List.combine file modified_words_file
    in 
    let result =
    let valid_word_check (empls) (tuple') =
    let rec look_for (w, sw) lst = 
    match lst with 
      | [] -> None
      | (w', sw')::rest -> if w' = sw && List.length(explode w') = 4 && List.length(explode w) = 6 then Some (w', w) else look_for(w,sw) rest 
      in 
      match look_for tuple' combined_file with
      | Some pair -> pair::empls
      | None -> empls
      in
    List.fold_left (valid_word_check) [] combined_file
    in 
    result
    
    