# Homework 4: Reasoning about Correctness.

by <John Lei>

# Problem 1
# Base Case:
Show: prod(l1 @ l2) = prod [] * prod l2

prod([] @ l2)
= prod l2
    by properties of lists and @
= 1 * prod l2
    by arithmetic
= prod [] * prod l2
    by def of prod

# Inductive Case:
Show: prod((y::ys) @ l2) = prod(y::ys) * prod l2 
Inductive Hypothesis: prod(ys @ l2) = prod ys * prod l2
Using weak induction

prod((y::ys) @ l2)
= prod(y::(ys @ l2))
    by properties of lists and @
= y * prod(xs @ l2)
    by def of prod
= y * (prod ys * prod l2)
    by inductive hypothesis
= (y * prod ys) * prod l2
    by arithmetic
= prod(y::ys) * prod l2
    by def of prod

# Problem 2
# Base Case:
Show: sum(inc_all []) = length [] + sum []

inc_all [] 
= []
    by def of inc_all
Since inc_all [] = []
sum([]) 
= 0
    by def of sum
length[] 
= 0
    by def of length
sum [] 
= 0
    by def of sum
0 + 0
= 0
    by arithmetic
As sum(inc_all []) = 0 and length [] + sum [] = 0, LHS = RHS

# Inductive Case
Show: sum(inc_all y::ys) = length(y::ys) + sum(y::ys)
Inductive Hypothesis: sum(inc_all ys) = length(ys) + sum(ys)
Weak Induction

LHS:
= sum((y+1) :: inc_all ys)
    by def of inc_all
= (y+1) + sum(inc_all ys)
    by def of sum

RHS:
= 1 + length(ys)
    by def of length
= y + sum(ys) 
    by def of sum
= 1 + length(ys) + y + sum(ys)
    by combining the functions
= (y+1) + length(ys) + sum(ys)
    by arithmetic

LHS = (y+1) + sum(inc_all ys)
RHS = (y+1) + length(ys) + sum(ys)
Subtract (y+1) on both sides because of arithmetic

Now sum(inc_all ys) = length(ys) + sum(ys). Equals inductive hypothesis(by inductive hypothesis) thus inductive case is proven.

# Problem 3
# Base Case:
Show: map inc [] = inc_all []

RHS: 
inc_all [] 
= []
    by def of inc_all

LHS: 
map inc [] 
= []
    by def of map (doesn't matter what function is)

LHS = [] and RHS = [], LHS = RHS, Base case proven

# Inductive Case:

Show: map inc y::ys = inc_all y::ys
Inductive Hypothesis: map inc ys = inc_all ys

RHS:
inc_all y::ys 
= (y+1) :: inc_all ys 
    by def of inc_all

LHS:
map inc y::ys
= inc y :: map inc ys
    by def of map
= (y+1) :: map inc ys
    by def of inc

In form of map inc ys = inc_all ys, (y+1) doesn't matter due to how lists and append work, so LHS = RHS
By inductive hypothesis, inductive case is proven. 

# Problem 4
let rec product tree =
    match tree with
    |Empty -> 1
    |Node(v,tl,tr) -> v * product tl * product tr
# Base Case:
Show prod(to_list Empty) = product Empty

RHS:
product Empty
= 1
    due to def of product

LHS:
prod(to_list Empty)
= prod([])
    due to def of to_list
= 1
    due to def of prod

LHS = 1 and RHS = 1, so LHS = RHS. Thus Base Case is proven

# Inductive Case
Show: prod(to_list Node(v,tl,tr)) = product(Node(v,tl,tr))
Inductive Hypothesis: prod(to_list tl) = product(tl) and prod(to_list tr) = product(tr) Prove by weak induction

LHS:

= prod(to_list tl @ [v] @ to_list tr)
    by def of to_list
= prod(to_list tl) @ prod([v]) @ prod(to_list tr)
    by arithmetic

prod([v]) = v * prod[] = v * 1 = v
    by def of prod

= prod(to_list tl) @ v @ prod(to_list tr)
    by def of prod
= v @ prod(to_list tl) @ prod(to_list tr)
    by properties of lists
= v * prod(to_list tl) * prod(to_list tr)
    by Problem 1 as prod(l1 @ l2) = prod l1 * l2 and to_list returns a list

RHS:

= v * product tl * product tr
    by def of product

Divide by v on both sides so LHS = prod(to_list tl) * prod(to_list tr) and RHS = product(tl) * product(tr)

By the inductive hypothesis, both conditions are met, so inductive case is proven

# Problem 5:

let rec size tree = 
    match tree with 
    |Empty -> 0
    |Node(x,y,z) -> 1 + size y + size z

let size_r (tree: 'a tree) =
    let sizetree x y z = 1 + y + z
    in reduce tree 0 sizetree

let rec reduce (tree: 'a tree) (empty: 'b) (f: 'a -> 'b -> 'b -> 'b) : 'b = 
    match tree with
    | Empty -> empty
    | Node (v, t1, t2) -> f v (reduce t1 empty f ) (reduce t2 empty f)


# Base Case:
Show size Empty = size_r Empty

LHS:

= 0
    by def of size

RHS:

= reduce Empty 0 sizetree
    by def of size_r
= 0
    by def of reduce

Since LHS=RHS as they both equal 0, so base case is proven

# Inductive Case:
Show size(Node(v,tl,tr)) = size_r (Node(v,tl,tr))
Inductive hypothesis: size(tl) = size_r(tl) and size(tr) = size_r(tr)
Prove by weak induction

LHS:

= 1 + size(tl) + size(tr)
    by def of size
= size Node(v,t1,t2)
    by def of size


RHS: (basically work backwards)

= 1 + size_r t1 + size_r t2
    by inductive hypothesis and def of size

= sizetree v (reduce t1 0 sizetree) (reduce t2 0 sizetree)
    by def of reduce
= reduce sizetree 0 Node(v,tl,tr)
    by def of reduce
=size_r Node(v,tl,tr)
    by def of size_r

As LHS = size(Node(v,tl,tr)) and RHS = size_r(Node(v,tl,tr)), inductive case is now met. 

