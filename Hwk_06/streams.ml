(* Constructing lazy values in OCaml *)

(* Lazy datatypes and functions *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a 
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = 
  ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee


(* Some examples streams from files developed in class. *)
let rec from n =
  Cons ( n, delay ( fun () -> from (n+1) ) )

let ones =
  let rec mk_ones () = Cons (1, delay ( mk_ones ) )
  in mk_ones ()

let nats = from 1


(* Some helpful functions from files developed in class. *)
let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s :'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let rec take (n: int) (s: 'a stream) : 'a list =
  match n with
  | 0 -> []
  | _ -> (match s with
          | Cons (h, t) -> h :: take (n-1) (demand t) 
         )

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) -> 
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd 
     then Cons (hd, rest)
     else demand rest

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (h, t) -> Cons (f h, delay (fun () -> map f (demand t)))

let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (h1, t1), Cons (h2, t2) ->
     Cons (f h1 h2, delay (fun () -> zip f (demand t1) (demand t2)))


(* Below is a stream of factorials.  It uses, the same definition of
   factorials as we developed in class except that the built in
   multiplication operator is replaced by a function `mul_p` that
   still multiplies its arguments but prints out those arguments as
   well.  *)

let mul_p x y =
  let () = print_endline ("multiplying " ^ string_of_int x ^ " and " ^
                            string_of_int y ^ ".")
  in x * y

let rec factorials () =
  Cons (1, delay (fun () -> zip mul_p nats (factorials ())))

let facts = factorials ()

let () =
  assert (take 5 facts = [1; 1; 2; 6; 24])



(* Please write your solutions below. *)

let rec cubes_from (x: int) : int stream =
    Cons ( x*x*x, delay ( fun () -> cubes_from(x+1)  ) )

let cubes_from_map (x:int) : int stream =
  map(fun n -> n*n*n) (from x)

let cubes_from_zip (x:int) : int stream = 
  let first_zip = zip(fun a b -> a*b) (from x) (from x)
  in
  zip (fun b c -> b * c) first_zip (from x)


(* Another way to do facts', but I felt like it sort of cheated the homework
let mul x y = x*y

let rec noprintfact() = Cons (1, delay (fun () -> zip mul nats (noprintfact ())))

let factorials' () =
  Cons (1, delay (fun () -> zip mul_p nats (noprintfact())))

let facts' = factorials'()

facts' does not repeat mul_p as the calls on another function that makes its own factorials, but with out printing every step being [1;1;2;6;24 ...].
  Using that stream, we can non-recurisvely call on another function that call mul_p, but this will multiply the natural numbers with the factorial stream.
  This means that it will just print step by step since we are not printing anything for our recursive factorial function, and we are just multiplying two streams non recursively.
  Thus this method works.*)

let facts' =
  let hold = ref nats
  in
  let fact = Cons (1, delay (fun () -> zip mul_p nats (! hold)))
  in
  let () = hold := fact
  in
  fact
(*Help from sample programs in lazy.ml. Right now we are getting rid of delaying the factorial process. We use the holder varible so we can point it to something else.
 Hold is just the stream nats, but we will replace it in the Cons(). Before we return the Cons() (in this case fact), we need to change what hold is referencing to, which is the return value.
 So what we are zipping is nats and what are deferencing to give us factorials (fact). Basically this hold varible will just hold the previous result of the multiplying. This means we don't redo the previous steps as we will always have the result from previous multiplying which is stored in hold.
 This also makes a circular structure. This process at the end of the function will change what fact is derefencing to which doesn't repeat mul_p. *)

let sift (x:int) (ls:int stream) : int stream = 
  filter (fun a -> a mod x != 0) ls

let h (lst: 'a stream) : 'a = head lst

let t (lst: 'a stream) : 'a stream = tail lst

let rec sieve (ls: int stream) : int stream =
  Cons((h (ls)), delay (fun() -> sieve (sift (h ls)(t ls))))

let primes = sieve (from 2)

let () =
  print_string "Testing part 1 ... " ;
  try
  assert (take 5 (cubes_from 1) = [1; 8; 27; 64; 125]);
  assert (take 5 (cubes_from_map 1) = [1; 8; 27; 64; 125]);
  assert (take 5 (cubes_from_zip 1) = [1; 8; 27; 64; 125]);
  assert (take 3 (cubes_from 3) = [27; 64; 125]);
  assert (take 3 (cubes_from_map 3) = [27; 64; 125]);
  assert (take 3 (cubes_from_zip 3) = [27; 64; 125]);
  assert (take 5 facts' = [1; 1; 2; 6; 24]);
  assert (take 5 (sift 2 (from 3))= [3;5;7;9;11]);
  assert ( take 10 primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29] );
  print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg
