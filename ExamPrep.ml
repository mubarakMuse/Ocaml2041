match ((fun x -> x) [1;2]) with
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