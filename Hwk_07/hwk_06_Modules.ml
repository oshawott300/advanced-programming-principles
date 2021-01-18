open StreamModules

module type Hwk_06_Sig = sig
  type 'a stream

  val take: int -> 'a stream -> 'a list

  val from: int -> int stream
  val nats: int stream
  val cubes_from: int -> int stream
  val cubes_from_zip: int -> int stream
  val cubes_from_map: int -> int stream
  val facts: int stream
  val facts': int stream
  val primes: int stream
end

module Hwk_06(S: StreamSig) : Hwk_06_Sig = struct
   
   type 'a stream = 'a S.t

   let take = S.take

   let rec from n =
  S.Cons ( n, S.delay ( fun () -> from (n+1) ) )

  let rec cubes_from (x: int) : int stream =
    S.Cons ( x*x*x, S.delay ( fun () -> cubes_from(x+1)  ) )

    let cubes_from_map (x:int) : int stream =
        S.map(fun n -> n*n*n) (from x)

    let cubes_from_zip (x:int) : int stream = 
        let first_zip = S.zip(fun a b -> a*b) (from x) (from x)
        in
        S.zip (fun b c -> b * c) first_zip (from x)
    
    let nats = from 1

    let mul_p x y =
        let () = print_endline ("multiplying " ^ string_of_int x ^ " and " ^
                            string_of_int y ^ ".")
        in x * y
    
    let rec factorials () =
        S.Cons (1, S.delay (fun () -> S.zip mul_p nats (factorials ())))

    let facts = factorials ()
    
    let facts' =
        let hold = ref nats
        in
        let fact = S.Cons (1, S.delay (fun () -> S.zip mul_p nats (! hold)))
        in
        let () = hold := fact
        in
        fact
(*Help from sample programs in lazy.ml. Right now we are getting rid of delaying the factorial process. We use the holder varible so we can point it to something else.
 Hold is just the stream nats, but we will replace it in the Cons(). Before we return the Cons() (in this case fact), we need to change what hold is referencing to, which is the return value.
 So what we are zipping is nats and what are deferencing to give us factorials (fact). Basically this hold varible will just hold the previous result of the multiplying. This means we don't redo the previous steps as we will always have the result from previous multiplying which is stored in hold.
 This also makes a circular structure. This process at the end of the function will change what fact is derefencing to which doesn't repeat mul_p. *)

    
    let sift (x:int) (ls:int stream) : int stream = 
        S.filter (fun a -> a mod x != 0) ls 

    let rec sieve (ls: int stream) : int stream =
        S.Cons((S.head(ls)), S.delay (fun() -> sieve (sift (S.head ls)(S.tail ls))))

    let primes = sieve (from 2)






end