type 'a btree = Nil
              | Leaf of 'a
              | Fork of 'a btree * 'a * 'a btree

let rec insert_by func x tree = 
    match tree with
    |Nil -> Leaf x (*if empty, just add a leaf*)
    |Leaf v -> if (func x v) = 1 then Fork(Nil, v, Leaf x) 
    else if (func x v) = -1 then Fork(Leaf x, v , Nil) else Leaf v (*if it's just a leaf as the tree, compare the value and create a Fork depending if it's less than or greater than the compared value. If equal, just return the leaf*)
    |Fork(t1,v,t2) -> if (func x v) = 1 then Fork(t1,v, insert_by func x t2)
        else Fork(insert_by func x t1, v, t2) (*If adding to a Fork, traverse the list using compare and add to the Fork*)
 
 let from_list func ls = List.fold_left(fun a b -> insert_by func b a) Nil ls
  
  
let rec reduce tree n l func =
  match tree with
  |Nil -> n (*empty case*)
  |Leaf v -> l v  (*apply leaf function to the one value in the list*)
  |Fork(t1,v,t2) -> func (reduce t1 n l func) v (reduce t2 n l func) (*apply function to all values in Fork*)


let to_list tree =
  let append_one_ls y = y::[] (*just if there is a leaf, append that value to empty list*)
  in
  let append x y z = x @ [y] @ z (*if there is a Fork, append all values together for list*)
  in
  reduce tree [] (append_one_ls) (append) (*just like other reduce functions, need the tree, base case and functions to apply to the tree*)


let () = 
  print_string "Testing part 4 ... " ;
  try
    assert (insert_by compare 4 Nil = Leaf 4);
    assert (insert_by compare 2 (insert_by compare 4 Nil) =
              Fork (Leaf 2, 4, Nil));
    assert (insert_by compare 4 (insert_by compare 2 Nil) =
              Fork (Nil, 2, Leaf 4));
    assert (insert_by compare 4 (insert_by compare 4 Nil) = 
              insert_by compare 4 Nil);
    assert(insert_by compare 4 (Leaf 2) = Fork (Nil, 2, Leaf 4));
    assert(insert_by compare 4 (Leaf 5) = Fork (Leaf 4, 5, Nil));
    assert(insert_by compare 4 (Leaf 4) = Leaf 4);
    assert(insert_by compare 4 (Fork(Leaf 2,3 , Leaf 5)) = Fork (Leaf 2, 3, Fork (Leaf 4, 5, Nil)));
    assert (from_list compare [4;2;5;3;6;7;8] =
              Fork (Fork (Nil, 2, Leaf 3), 4,
                    Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8)))
                   ) 
           );
    assert(from_list compare [] = Nil);
    assert(from_list compare [2] = Leaf 2);
    assert(from_list compare [6;4;2;1] = Fork (Fork (Fork (Leaf 1, 2, Nil), 4, Nil), 6, Nil));
    assert(reduce (from_list compare [1;2;3]) 0 (fun a -> a) (fun a b c -> a+b+c)=6);
    assert(reduce (from_list compare [1;2;3]) 1 (fun a -> a) (fun a b c -> a*b*c)=6);
    assert (List.sort compare [4;2;5;3;6;7;8] =
              to_list (from_list compare [4;2;5;3;6;7;8]));
    assert(to_list (from_list compare []) = []);
    assert(to_list(from_list compare [4]) = [4]);

    assert(to_list(from_list compare [1;2;3;4;6;7;8;10;12;34;56]) = [1; 2; 3; 4; 6; 7; 8; 10; 12; 34; 56]);

          

    (* Add more asserts here as you need them *)
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg
