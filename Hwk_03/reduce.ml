type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

(* A sample tree containing ints *)
let int_tree : int tree =
  Node (3, 
        Node (1,
              Node (4, Empty, Empty), Empty), 
        Node (2, Empty, Empty) 
       )

(* A sample tree containing strings *)
let str_tree : string tree = 
  Node ("love ", 
        Node ("really ", 
              Node ("I ", Empty, Empty), Empty), 
        Node ("OCaml!", Empty, Empty) 
       )
let multi_int_tree : int tree =
    Node (3, 
        Node (1,
              Node (4, Node(5,Empty,Empty), Node(7,Empty,Empty)), Empty), 
        Node (2, Node(1,Empty,Empty), Empty) 
       )

let zero_tree = 
    Node (3, 
        Node (0,
              Node (4, Empty, Empty), Empty), 
        Node (2, Empty, Empty) 
       )
let multi_str_tree : string tree = 
  Node ("love ", 
        Node ("really ", 
              Node ("I ", Node("hate ",Empty,Empty), Node("hate ", Empty,Empty)), Empty), 
        Node ("OCaml!", Node("hate ", Empty,Empty), Empty) 
       )
let empty_tree = Empty


let ints_tree: int list tree =
  Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )

let multi_ints_tree: int list tree =
  Node ([1;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Node([10;11;12],Empty,Empty), Node([5;6;9],Empty,Empty))
             ),
        Node ([],
              Node ([1;6], Node([10;14;5],Empty,Empty), Empty),
              Node ([9;2;8],Node([4;5;6],Empty,Empty),Empty)
             )
       )

let multi_zero_tree: int list tree =
  Node ([0;3],
        Node ([4;5;6], 
              Empty,
              Node ([], Node([10;11;12],Empty,Empty), Node([5;6;9],Empty,Empty))
             ),
        Node ([],
              Node ([1;6], Node([10;14;5],Empty,Empty), Empty),
              Node ([9;2;8],Node([4;5;6],Empty,Empty),Empty)
             )
       )

let strs_tree: string list tree = 
  Node (["Ocaml!  "; "It "; "must "; "be "],
        Node (["do "; "love "], 
              Node (["I "; "really "], Empty, Empty), Empty), 
        Node (["your "; "favorite "; "too!"], Empty, Empty) 
       )

let multi_strs_tree: string list tree = 
  Node (["Ocaml!  "; "It "; "must "; "be "],
        Node (["do "; "love "], 
              Node (["I "; "really "], Node(["hate ";"hate "], Empty, Empty), Empty), Empty), 
        Node (["your "; "favorite "; "too!"], Node(["hate ";"hate "; "hate "], Empty, Empty), Empty) 
       )

let int_elem_tree = Node(1, Empty, Empty)

let str_elem_tree = Node("Hi", Empty, Empty)

let rec reduce (tree: 'a tree) (empty: 'b) (f: 'a -> 'b -> 'b -> 'b) : 'b = (*got this from the public repo, but it will apply the values to all nodes in the tree*)
    match tree with
    | Empty -> empty
    | Node (v, t1, t2) -> f v (reduce t1 empty f ) (reduce t2 empty f)

let size (tree: 'a tree) =
    let sizetree x y z = 1 + y + z (*function for the reduce*)
    in reduce tree 0 sizetree (*parameters are the base case, tree and function to apply to all the values*)

let sum tree =
    let addelem x y z = x + y + z
    in reduce tree 0 addelem

let product tree =
    let multielem x y z = x*y*z
    in reduce tree 1 multielem

let charcount tree =
    let charelem x y z = String.length(x) + y + z
    in reduce tree 0 charelem

let concat tree =
    let concatelem x y z = y ^ x ^ z
    in reduce tree "" concatelem

let list_tree_size tree = 
    let length ls = List.fold_left(fun count elem -> count + 1) 0 ls (*still need the fold for all lists in the tree*)
    in
    let listsizetree x y z = length(x) + y + z (*still same process as other reduce functions, need to apply to all nodes in tree*)
    in reduce tree 0 listsizetree

let list_tree_sum tree = 
    let sum ls = List.fold_left(fun a b -> a + b) 0 ls
    in
    let listsumtree x y z = sum(x) + y + z
    in reduce tree 0 listsumtree

let list_tree_product tree = 
    let product ls = List.fold_left(fun a b -> a*b) 1 ls
    in
    let listproducttree x y z = product(x) * y * z
    in reduce tree 1 listproducttree

let list_tree_charcount tree = 
    let charcount ls = List.fold_left(fun count elem -> String.length(elem) + count) 0 ls
    in
    let listcharcounttree x y z = charcount(x) + y + z
    in reduce tree 0 listcharcounttree

let list_tree_concat tree = 
    let concat ls = List.fold_left(fun a b -> a ^ b) "" ls
    in
    let listconcattree x y z = y ^ concat(x) ^ z
    in reduce tree "" listconcattree

let () = 
  print_string "Testing part 3 ... " ;
  try
    
    assert (size str_tree = 4);
    assert (size int_tree = 4);
    assert (size empty_tree = 0);
    assert (size int_elem_tree = 1);
    assert (size str_elem_tree = 1);
    assert (size strs_tree = 4);
    assert (size ints_tree = 6);
    assert(size multi_int_tree = 7);
    assert (sum int_tree = 10);
    assert (sum empty_tree = 0);
    assert (sum int_elem_tree = 1);
    assert(sum multi_int_tree = 23);
    assert (product empty_tree =1);
    assert (product int_elem_tree = 1);
    assert (product int_tree = 24);
    assert(product multi_int_tree = 840);
    assert(product zero_tree = 0);
    assert(charcount empty_tree = 0);
    assert(charcount str_elem_tree = 2);
    assert (charcount str_tree = 20);
    assert(charcount multi_str_tree = 35);
    assert (concat empty_tree = "");
    assert (concat str_elem_tree = "Hi");
    assert (concat str_tree = "I really love OCaml!");
    assert (concat multi_str_tree = "hate I hate really love hate OCaml!");
    assert (list_tree_size strs_tree = 11);
    assert (list_tree_size multi_ints_tree = 22);
    assert (list_tree_sum ints_tree = 45);
    assert (list_tree_sum multi_ints_tree = 142);
    assert (list_tree_product ints_tree = 311040);
    assert (list_tree_product multi_ints_tree = 9311791104000000);
    assert (list_tree_product multi_zero_tree = 0);
    assert (list_tree_charcount strs_tree = 54);
    assert (list_tree_charcount multi_strs_tree = 79);
    assert (list_tree_concat strs_tree = 
              "I really do love Ocaml!  It must be your favorite too!");
    assert(list_tree_concat multi_strs_tree = "hate hate I really do love Ocaml!  It must be hate hate hate your favorite too!");
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg