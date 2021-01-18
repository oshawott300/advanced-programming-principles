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




let add_qu (file:string list) : string list = List.map(fun string -> "qu"^string) file

let rec qu_quiz (str:string) : (string * string) list =
    let file = read_file str 
    in
    let exploded_file = List.map(fun s -> explode s) file
    in
    let sorted_file = List.map (fun s -> implode s) (List.map(fun s -> List.sort compare s) exploded_file)
    in 
    let qu_exploded_file = List.map(fun s -> explode s) (add_qu file)
    in
    let added_qu_file = List.map (fun s -> implode s) (List.map(fun s -> List.sort compare s) (qu_exploded_file))
    in
    let combined_tuples = List.combine file (List.combine sorted_file added_qu_file) 
    in
    let result = 
    let valid_word_check (empls) (tuple') =
    let rec look_for (w, (sw, swz)) lst = 
    match lst with 
      | [] -> None
      | (w', (sw', swz'))::rest -> if sw' = swz then Some (w, w') else look_for(w,(sw,swz)) rest 
      in match look_for tuple' combined_tuples with
      | Some pair -> pair::empls
      | None -> empls
      in
    List.rev(List.fold_left (valid_word_check) [] combined_tuples)
    in 
    result

    
  
  