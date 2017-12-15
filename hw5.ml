(* for this state i see that theres going to a list for to accumalte to new reversed list
i also sse that theres an input list
theres no other varibles or counter that i have to account for so 
those are the only to element that make up my state*)

type state = int list * int list
(* get and put functions for each of the state variables *)
let getRev (rev,l) = rev
let getL (rev,l) = l (* l is the input list *)
let getLhead (rev,(h::t)) = h(* return the head of the input list in the state*)
let getLtail (rev,(h::t)) = t (* return the tail of the input list in the state*)
let putRev exp s =
	let (rev,l) = s in (exp s, l)
let putL exp s =
	let (rev,l) = s in (rev, exp s) (* no need to update the head and tail for the given operations
	otherwise i would have made a put fucntion for both *)

(* all of the seq, ifstat and while stat where given by instructor*)
let seq stat1 stat2 =
  	 fun s -> (stat2 (stat1 s))

let ifstat exp stat1 stat2 = (* if else statment *)
    fun s -> if (exp s) then (stat1 s)
             else (stat2 s)

let rec whilestat exp stat = (* while loop *)
   fun s ->
      ifstat exp
             (seq stat (whilestat exp stat))
             (fun x -> x) s

let emptyList = fun s -> [] 
let ltail = (fun s -> (getLtail s)) (* returns tail of imput list as an expression*)
let lhead_Plus_rev = (fun s -> (getLhead s):: (getRev s)) (* append head of the input list to
 whatever the reverse list has in it then and returns as expression *)
let l_not_empty =  (fun s -> not ((getL s) == [])) (* checks if the input is empty to stop to the while loop*)

let revprog = 
		(seq (putRev emptyList) 
			(whilestat l_not_empty 
				(seq (putRev lhead_Plus_rev)
					(putL ltail))))
let revlist l1 =  getRev (revprog ([],l1))

(* 2 *)
(* the state for this program is dependent on the varaibles 
	thus i have a state varaible for i and sum and n(input) *)

type state = int * int * int

(* i changed the sequencing of the while loop to do the exp first 
then check the condtion and run the loop again to cond fails *)
let rec dostat exp stat = 
   fun s -> (seq (stat )
      (ifstat exp (dostat exp stat) (fun x -> x))s)

(* get and put functions for each of the state variables *)
let getI (i,sum,n) = i
let getSum (i,sum,n) = sum
let getN (i,sum,n) = n
let putI exp s =
	let (i,sum,n) = s in (exp s, sum,n)
let putSum exp s =
	let (i,sum,n) = s in (i, exp s,n)
let putN exp s =
	let (i,sum,n) = s in (i,sum, exp s)

let seq stat1 stat2 =
  	 fun s -> (stat2 (stat1 s))

let ifstat exp stat1 stat2 =
    fun s -> if (exp s) then (stat1 s)
             else (stat2 s)

let zero = fun s -> 0 (* zero as an expression *)
let i_plus_One = (fun s -> (getI s) + 1)(* adding 1 to i *)
let sum_plus_i = (fun s ->  (getSum s) + (getI s) )(* adding the sum to i to increnmt the sum *)
let i_lessThan_n = (fun s -> (getI s) < (getN s)) (*condition statemnet for my while loop*)

let sumup = 
	(seq (putI zero) 
		(seq (putSum zero) 
			(dostat (i_lessThan_n) 
				(seq (putI i_plus_One) 
					(putSum sum_plus_i) ))))

let sumToN n = getSum (sumup (0,0,n))

(*
(Problem 3 Part 1)

For all n in int fib’ n m f s  = the nth fibonacci number 
f = mth fib number 
s = (m+1)th number

(Problem 3 Part 2)
Base case: P(0) when n=m then n-m = 0 then using the def of fib’  we that it  evals to f thus returning the mth fib number which proves the base case since m = n

When n> m then n-m >= 1 n-m= k+1 for k>=0

Inductive step:

Assume P(k) for all n in int fib’ n m f s  = the nth fibonacci number when n-m = k
To show p(k+1) for all n in int fib’ n m f s  = the nth fibonacci number when n-m = k-1
And n>m in both cases

Using the def of fib’ we that in the first iteration of the function it returns fib’  n m+1
s f+s so when it increments our m we find that the relationship of n and m becomes n-(m=1) = k +1 , or n-m = k
which i inductive hypotheses and if the other properties we mention above hold then it will return ( n-1  fibonacci number + n-2  fibonacci number) thus proving P(k+1)

(Problem 3 Part 3)
With the preset of m f and s all to 1 we can see that 
1-	n =1 then it would return 1 which is what what we want
2-	n=2 then it runs through once and replaces f with s but that still return 1 which is what we want 
3-	n>2 run through many times need and keep tracking f and s and makes sure that they are increment correct and start from the adding up the 1th and 2nd fib number and build up from there



(Problem 4 Part 1a)
For all L1 in ‘a list P(L1) where P(L1) is the property: 
For all L2 in ‘a list (rev L1 L2 = L1^R + L2)
(Problem 4 Part 1b)
Base case: 
 P([]) for all L2 in ‘a list (rev [] L2 = []^R + L2)
LHS: using the def of rev we see that it can be simplified to whatever L2 evaluates to
RHS : the reverse of the an empty list is also empty and so appending L2 to it will would simplify to whatever l2 evaluates to 
thus proving that LHS and RHS evaluate to to the same proving our base case
inductive step:
Assume P(L1)
for all L2 in ‘a list rev L1 L2 = L1^R + L2
to show:  
for all x’ in ‘a for all L2 in ‘a list rev L1 L2 = (x’::L1)^R + L2
we choose an x’ in ‘a we show this to hold below
LHS:
Using the def of rev we can simplify the LHS to rev L1 (x’::L2)
Using the inductive hypothesis we can see that the LHS evaluates to whatever (L1^R + x’::L2) evaluates to 

RHS: (x’:: L1)^R + L2 = L1^R + (x’:: L2)

Thus the RHS and LHS evaluates to the same thus proving P(x’:: L1)
(Problem 4 Part 1c)
For L in a’ list reverse L evaluates to whatever rev L [] and using the property we just proved above we can see that if L is [] it would return [] other wise it would always evaluate to (L^R + []) which is just always L^R (reverse of L)
(Problem 4 Part 2a)

1 -for  L1 in a’ list P(L1) where P(L1) is the property
for  L2 in a’ list (length (rev L1 L2)) evaluates to (length L1 + length L2)
(Problem 4 Part 2b)

Base case: P([])
for  L2 in a’ list (length (rev [] L2)) evaluates to (length [] + length L2)

LHS:
 using the rev property we proved we see that LHS  evals to whatever length (L2) evaluates to
RHS: using the def of length we see that RHS evaslute to whatever 0+length (L2) evaluates so RHS = LHS

Inductive step:
Assume P(L1)  for  L2 in a’ list (length (rev L1 L2)) evaluates to (length L1 + length L2)
To show for all x’ in ‘a  for  L2 in a’ list (length (rev x’::L1 L2)) evaluates to (length  x’::L1 + length L2)

we choose an x’ in ‘a we show this to hold below
LHS: using what we proved about rev above we simplify the LHS to
 length ((L1^R + x’::L2))  rearranging  this to length ((x’::(L1^R + L2)) now using the def we can see that this evaluates to whatever ( length (L1^R + L2)  + 1) evaluates to

RHS:  the RHS using the def of length simplifies and evaluates to whatever (length L1^R ) +1 (length L2) evaluates to 

Using the inductive hypothesis, we can see that LHS evaluates to whatever the RHS evaluates to 

(Problem 4 Part 2c)

For L in a’ list length (reverse L) evaluates to whatever length (rev L []) and using the property we just proved above we can see that if L is [] it would return length ( [] ) which is zero
 other wise it would always evaluate to Length  (L^R + []) which is just always evaluates to length ( L^R )(length of  L)

(problem 5)

For any given tree t and for any given data item x suppose that (insert t x) evaluates to the tree t'. Then (find t' x') evaluates to true for any data item x' of the type stored in t 
only if either x' is x or (find t x') evaluates to true.

base case : P(Empty)
for all x in 'a if insert (Empty x) evals to t' Then (find t' x') evaluates to true 
for any data item x' of type 'a only if x' is x or (find empty x') evaluates to true.

Observing that t' = Node (x,Empty,Empty), is easy to see that it would only true if x' is x 
since (find empty x') always evalutes to false

inductive step:
let say t is of form Node (i,l,r) assume P(l) and P(r) to hold
to show p(t) which is for for all x in 'a if insert (t x) evals to t' Then (find t' x') evaluates to true 
for any data item x' of type 'a only if x' is x or (find t x') evaluates to true.

trivialy we can see why if x' is x then (find t' x') since we added x to right before 
based on i not being x we have 2 case:

for the case i<x then using the fuction definition we insert x in to l so t' = Node (i,l', r) where  l' = insert l x 
by induction we Then see that (find l' x') evaluates to true 
for any data item x' of type 'a only if x' is x or (find l x') evaluates to true.
therefore if x' is x then find t' x is also true and if (find l x') is true then find t' x is 
also true since its l is its subtree


for the case i>=x its the smae logic as previous but now using the fuction definition we insert x in to r instead and
 so t' = Node (i,l, r') where  r' = insert r x by induction we Then see that (find r' x') evaluates to true 
for any data item x' of type 'a only if x' is x or (find l x') evaluates to true.
therefore if x' is x then find t' x is also true and if (find r x') is true then find t' x is 
also true since its r is its subtree

thus proving our property P(t)




*)