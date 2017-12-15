(* match ((fun x -> x) [1;2]) with
| [] -> "dfsas"
| (h::t) -> "sdasdas"

match [(1,"string")] with
| [] -> []
| ((y,v)::t) -> if (1= y) then v else t
let x = 1 in  y = x + 1 in x + y

let rec reverse l =
match l with
| [] -> []
| (h::t) -> reverse t:: h
let f n0 = 
	let rec f' n v = 
	if (n<=0) then v
	else if (n mod 2 = 0) then f'(n/2) (2*v + v*v)
	else f' (n-1) (3*v) 
in f' n0 1

type intorstr = Int of int | Str of string
let rec separate a = 
	match a with
	|  [] -> ([],[])
	| h::t -> 
		match h with
		| Int i -> let i2 = separate t in (i2,i::[])
		| Str a -> let a2 = separate t in (a::[],a2)

type expr =
Id of string | Int of int
| Plus of expr * expr | Minus of expr * expr

let simplify a = 
	match a with 
	| Id s -> Id s
	| Int i -> Int i
	| Plus (e1,e2) -> if e1 land e2 = int then e1+e2
						else Plus (simplify e1,simplify e2)
	| Minus (e1,e2) -> if e1 land e2 = int then e1-e2
						else Minus (simplify e1,simplify e2)

(*--------------------------------------- midterm 2 *)

let rec map2 f l1 l2 = 
	match (l1,l2) with
	| (h1::t1,h2::t2) -> (f h1 h2) :: map2 f t1 t2
	| _-> []

let divide_list f l1 =

	let rec divide_list2 f' l1' l2 l3 = 
		match l1' with
		| [] -> (l2,l3)
		| h::t -> if (f h) then (divide_list2 (f') (t) (h::l2) (l3) )
				else (divide_list2 (f') (t) (l2) (h::l3))
in divide_list2 f l1 [] [] *)

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let rec treemap tree f = 
	match tree with
	| Empty -> Empty
	| Node(i,l,r) -> Node((f i),treemap l f, treemap r f )

let seq stat1 stat2 =
  	 fun s -> (stat2 (stat1 s))

let ifstat exp stat1 stat2 =
    fun s -> if (exp s) then (stat1 s)
             else (stat2 s)
let rec repeatstat cond stat =
	(fun state -> 
		(seq (stat) (ifstat (cond)  
			(fun stat -> stat)(repeatstat cond stat)))state)
let rec dostat exp stat =
   fun s -> (seq (stat )
      (ifstat exp (dostat exp stat) (fun x -> x))s)

type state = int * int * int

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

let zero = fun s -> 0
let i_plus_One = (fun s -> (getI s) + 1)
let sum_plus_i = (fun s ->  (getSum s) + (getI s) )
let i_greatereq_n = (fun s -> not((getI s) < (getN s)))

let sumup = 
	(seq (putI zero) 
		(seq (putSum zero) 
			(repeatstat (i_greatereq_n) 
				(seq (putI i_plus_One) 
					(putSum sum_plus_i) ))))

let sumToN n = getSum (sumup (0,0,n))
(* let rec cont_fib n c = 
	match n with
	| 1 -> c 1
	| 2 -> c 1
	| n -> cont_fib (n-2) (fun y -> c (cont_fib (n-1) (fun x -> c (x+y))))

type 'a btree =
    Empty
  | Node of 'a * 'a btree * 'a btree

let rec cont_insert tree i c =
	match tree with
	| Empty -> c (Node(i,Empty,Empty))
	| Node(i',l,r) -> 
	if (i<i') then cont_insert l i (fun x -> c(Node(i',x,r)))
				else cont_insert r i (fun x -> c(Node(i',l,x)))
let t = Node(4,Node(3,Empty,Empty),Empty) *)
(*
