(* This file contains a few helper functions and type declarations
   that are to be used in Homework 2. *)

(* Place part 1 functions 'take', 'drop', 'length', 'rev',
   'is_elem_by', 'is_elem', 'dedup', and 'split_by' here. *)

(*Is of type 'a list -> int*)
let length ls = List.fold_left(fun count elem -> count + 1) 0 ls

let is_elem_by func a ls =
    List.fold_left (fun b x -> b || x = a) false ls

let is_elem a lst= is_elem_by (=) a lst

let dedup ls = List.fold_right(fun a ls -> if is_elem a ls then ls else a :: ls) ls []

let rev lst = List.fold_left (fun a b -> b::a) [] lst

(*Has type char list -> char list list list. It'll just fold up the function where if it does equal the splitter, adds it to a list, else start a new list *)
let split_by func ls splitter = 
  let add (start,next) x =
    if (is_elem x splitter) then (start@[next],[])
    else (start, next@[x])
    in
  let empls = ([],[])
  in
  let (x,y) = List.fold_left add empls ls
  in 
  rev(rev (x@[y]))

let rec take n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then x::take (n-1) xs else []

let rec drop n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then drop (n-1) xs else l

type word = char list

type line = word list



let lines ls = split_by (=) ls ['\n']

let words ls = split_by (=) ls ['.'; '!'; '?'; ','; ';'; ':'; '-'; ' ']


let convert_to_non_blank_lines_of_words file =
    let file_divide = List.map words (lines file)
    in 
    let rid_empwords  = List.map (List.filter ( (!=) [] )) file_divide
    in
    let rid_emplines  = List.filter ( (!=) [] ) rid_empwords
    in rid_emplines





(* Some functions for reading files. *)
let read_file (filename:string) : char list option =
  let rec read_chars channel sofar =
    try 
      let ch = input_char channel
      in read_chars channel (ch :: sofar)
    with
    | _ -> sofar
  in
  try 
    let channel = open_in filename
    in 
    let chars_in_reverse = read_chars channel []
    in Some (rev chars_in_reverse)
  with
    _ -> None

let stanza_length = 6
let poem_length = 24

type result = OK 
	    | FileNotFound of string
	    | IncorrectNumLines of int 
	    | IncorrectLines of (int * int) list
	    | IncorrectLastStanza

let find_line num ls = 
  let line1 = take (num+1) ls
  in
 drop(num)(line1)

let loop ls elem = match ls with
    |[] -> [(elem,1)]
    |(_, x):: _ -> (elem, x+1)::ls

let line_dictionary ls = rev (List.fold_left loop [] ls)
(*I know recursion can't be used, but honestly, have no idea how to combine the lists into one big list without it*)
let flatten ls =
  let rec flat acc = function
    | []          -> acc
    | []:: rest -> flat acc rest
    | (x::y)::rest -> flat (x::acc) (y::rest) in
  List.rev (flat [] ls)
(*Same with sorting a list lol*)
let rec sort ls =
  let rec sorting = function
    | x::y::xs when x > y -> y :: sorting (x :: xs)
    | x :: y :: xs -> x :: sorting (y :: xs)
    | ls -> ls
  in
  let a = sorting ls in
    if a = ls then a
    else sort a

let lowercase ls = List.map (List.map (List.map Char.lowercase_ascii)) (convert_to_non_blank_lines_of_words ls)

let get_firstpair ls1 ls2 = flatten(flatten(ls1::ls2::[]))

let first_pair_stanza_check ls stanza_linenumber = 
  let first_stanzaline = 0
  in 
  let third_stanzaline = 2
  in
  let firstline = flatten(find_line (first_stanzaline + stanza_linenumber) (List.map (List.map (List.map Char.lowercase_ascii)) (convert_to_non_blank_lines_of_words ls)))
  in
  let thirdline = flatten(find_line (third_stanzaline + stanza_linenumber) (List.map (List.map (List.map Char.lowercase_ascii)) (convert_to_non_blank_lines_of_words ls)))
  in sort(get_firstpair firstline thirdline)

let get_lastpair ls1 ls2 = flatten(flatten(ls1::ls2::[])) 

let last_pair_stanza_check ls stanza_linenumber =
  let fifth_stanzaline = 4
  in 
  let sixth_stanzaline = 5
  in
  let fifthline = flatten(find_line (fifth_stanzaline + stanza_linenumber) (List.map (List.map (List.map Char.lowercase_ascii)) (convert_to_non_blank_lines_of_words ls)))
  in
  let sixthline = flatten(find_line (sixth_stanzaline + stanza_linenumber) (List.map (List.map (List.map Char.lowercase_ascii)) (convert_to_non_blank_lines_of_words ls)))
  in sort(get_lastpair fifthline sixthline)

let check_pairs ls linenumber = 
let ls1 = first_pair_stanza_check ls linenumber
in 
let ls2 = last_pair_stanza_check ls linenumber
in
if(ls1 = ls2) then true else false


let where_in_stanza ls stanza_number = 
  if(stanza_number = 0) then check_pairs ls 0
  else if(stanza_number = 6) then check_pairs ls 6
  else check_pairs ls 12

let find_valcheck ls appendls a b = if(find_line a ls <> find_line b ls) then (a+1,b+1)::appendls else appendls
let first_stanza_same ls empls num = if(where_in_stanza ls num = false && empls = []) then (5,6)::empls else empls

let second_stanza_same ls empls num a b c d = if(where_in_stanza ls num = false && (is_elem (a,b) empls = false || is_elem(c,d) empls = false)) then (11,12)::empls else empls

let third_stanza_same ls empls num a b c d = if(where_in_stanza ls num = false && (is_elem (a,b) empls = false || is_elem(c,d) empls = false)) then (17,18)::empls else empls

let equal_check ls1 ls2 = if(ls1 = ls2) then true else false

let poem_check ls =
  let truels = lowercase ls
  in
  let outcome = []
  in
  let same_first_pair = find_valcheck truels outcome 0 1
  in
  let same_second_pair = find_valcheck truels same_first_pair 2 3
  in
  let checking_firststanza = first_stanza_same ls same_second_pair 0
  in
  let same_third_pair = find_valcheck truels checking_firststanza 6 7
  in 
  let same_fourth_pair = find_valcheck truels same_third_pair 8 9
  in
  let checking_secondstanza = second_stanza_same ls same_fourth_pair 6 7 8 9 10
  in
  let same_fifth_pair = find_valcheck truels checking_secondstanza 12 13
  in
  let same_sixth_pair = find_valcheck truels same_fifth_pair 14 15
  in
  let checking_thirdstanza = third_stanza_same ls same_sixth_pair 12 13 14 15 16
  in
  rev(checking_thirdstanza)

let getrid_dedup ls = dedup ls
let getrid_laststanza ls = drop 18 ls
let getrid_everythingbutlaststanza ls = take 18 ls


let last_stanza_check ls = 
  let truels = lowercase ls
  in
  let get_allwords_from_laststanza = sort(getrid_dedup(flatten(getrid_everythingbutlaststanza truels)))
  in
  let get_allwords = sort(getrid_dedup(flatten(getrid_laststanza truels)))
  in 
  equal_check get_allwords get_allwords_from_laststanza

(*This is a comment*)
let paradelle file = 
  let go_file = read_file file in 
    match go_file with
      |None -> FileNotFound file 
      |Some ls -> let amountoflines = length(convert_to_non_blank_lines_of_words ls) in
     if poem_length <> amountoflines then IncorrectNumLines amountoflines
      else if last_stanza_check ls = false then IncorrectLastStanza
        else if poem_check ls <> [] then IncorrectLines (poem_check ls)
        else OK





