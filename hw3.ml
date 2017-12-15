(*  Solution for Problem 1 *)
let divide_list f list1 = 
let rec divide_list' f' list1' tlist flist = (* helper function *)
	match list1' with
	| []-> (tlist , flist)
	| h::t -> if (f h) then divide_list' f t (h::tlist) (flist) 
				else divide_list' f t (tlist) (h::flist) 
in divide_list' f list1 [] []

(*  Solution for Problem 2 *)
type 'a btree =
    Empty
  | Node of 'a * 'a btree * 'a btree

let cont_insert tree i c =
	match tree with
	| Empty -> c Node (i,Empty,Empty)
	| Node(i',l,r) -> 
	if (i<i') then cont_insert l i (fun x -> c(Node(i,x,r)))
				else cont_insert r i (fun x -> c(Node(i,l,x)))

let rec treemap tree f = 
	match tree with
	| Empty-> Empty
	| Node (a,l ,r)-> Node ((f a), (treemap l f) ,(treemap r f))

(*  Solution for Problem 3 
 1) Reduce Function :
		1-  let statement =  Reduce : 'a -> 'b -> c' -> 'd
		2-  1st Match = Reduce : 'a -> 'b list -> c' -> 'c
		3-  2nd Match = Reduce : ('b -> 'c -> 'c) -> 'b list -> c'
			Final type Reduce : ('b -> 'c -> 'c) -> 'b list -> c'

    2) fora112 function
    	1-  let statement = fora112: 'a -> 'b -> c' -> 'd
    	2-  1st Match =  fora112: 'a -> 'b list  -> 'c List -> ' bool
    	3-  4th Match = fora112: ('b -> 'c -> bool) -> 'b list -> 'c list -> bool 

    		Final Type fora112: ('b -> 'c -> bool) -> 'b list -> 'c list -> bool 
*)

let rec reduce f lst u =
     match lst with
     | [] -> u
     | (h::t) -> f h (reduce f t u)
let rec accumulate f lst u =
     match lst with
     | [] -> u
     | (h::t) -> accumulate f t (f h u)

let append l1 l2 = reduce (fun x y -> x::y ) l1 (l2)
let reverse l1 = accumulate (fun x y -> x::y  ) l1 []
let filter f l1 = reduce (fun x y -> if (f x) then x::y else y) l1 []

(*  Solution for Problem 4
	let append l1 l2 = reduce (fun x y -> x::y ) l1 (l2)
	let reverse l1 = accumulate (fun x y -> y::x ) l1 []
	let filter f l1 = reduce (fun x y -> if (f x) then x::y else y) l1 []
*)

(*  Solution for Problem 5 *)
type expr =
       Id of string                     (* for identifiers *)
     | Int of int                       (* for integers *)
     | True                             (* for the boolean value true *)
     | False                            (* for the boolean value false *)
     | Plus of expr * expr              (* for exp1 + exp2 *)
     | Minus of expr * expr             (* for exp1 - exp2 *)
     | Times of expr * expr             (* for exp1 * exp2 *)
     | Div of expr * expr               (* for exp1 / exp2, division being for integers *)
     | Lss of expr * expr               (* for exp1 < exp2 *)
     | Eq of expr * expr                (* for exp1 = exp2, = being equality comparison *)
     | Gtr of expr * expr               (* for exp1 > exp2 *)
     | And of expr * expr               (* for exp1 && exp2 *)
     | Or of expr * expr                (* for exp1 || exp2 *)
     | Not of expr                      (* for not exp *)
     | Cond of expr * expr * expr       (* for if exp1 then exp2 else exp3 *)
     | Let of string * expr * expr      (* for let  = exp1 in exp2 *)
     | Fun of string * expr             (* for fun x -> exp *)
     | App of expr * expr               (* for (exp1 exp2) *)
 
let rec freeIn a s = 
	match a with
	| Id x -> if x = s then true else false
	| Int _ -> false
	| True| False -> false
	| Plus (e1,e2)  -> (freeIn e1 s)  || (freeIn e2 s)
	| Minus (e1,e2)  -> (freeIn e1 s)  || (freeIn e2 s)
	| Times (e1,e2)  -> (freeIn e1 s)  || (freeIn e2 s)
	| Div (e1,e2)  -> (freeIn e1 s)  || (freeIn e2 s)
	| Lss (e1,e2)  -> (freeIn e1 s)  || (freeIn e2 s)
	| Eq (e1,e2)  -> (freeIn e1 s)  || (freeIn e2 s)
	| Gtr (e1,e2)  -> (freeIn e1 s)  || (freeIn e2 s)
	| And (e1,e2)  -> (freeIn e1 s)  || (freeIn e2 s)
	| Or(e1,e2)  -> (freeIn e1 s)  || (freeIn e2 s)
	| Not (e1)  -> (freeIn e1 s) 
	| Cond (e1,e2,e3)  -> (freeIn e1 s)  || (freeIn e2 s) || (freeIn e3 s)
	| App (e1,e2)  -> (freeIn e1 s)  || (freeIn e2 s)
	| Let(e1,e2,e3) -> if e1 = s then (freeIn e2 s) else if (e1 <> s) then (freeIn e2 s) else false
	| Fun(e1,e2)-> if (e1<>s) then (freeIn e2 s) 
		else false

let namecounter = ref 0
let newname () =
     ( namecounter := !namecounter + 1; "var" ^ string_of_int !namecounter)

let rec subst a b c = 
	match a with
	| Id i -> if (freeIn (Id i) b) then c else Id i
	| Int n -> a
	| True -> a
	| False -> a
	| Plus (e1,e2) -> 
		(match (freeIn e1 b ,freeIn e2 b) with
		| (true, true) -> Plus (subst e1 b c,subst e2 b c)
		| (true, false) -> Plus (subst e1 b c,e2)
		| (false, true) -> Plus (e1,subst e2 b c)
		| (false,false) -> Plus (e1,e2) 
		)
	| Minus (e1,e2) -> 
		(match (freeIn e1 b ,freeIn e2 b) with
		| (true, true) -> Minus (subst e1 b c,subst e2 b c)
		| (true, false) -> Minus (subst e1 b c,e2)
		| (false, true) -> Minus (e1,subst e2 b c)
		| (false,false)  -> Minus(e1,e2) 
		)
	| Times (e1,e2) -> 
		(match (freeIn e1 b ,freeIn e2 b) with
		| (true, true) -> Times (subst e1 b c,subst e2 b c)
		| (true, false) -> Times (subst e1 b c,e2)
		| (false, true) -> Times (e1,subst e2 b c)
		| (false,false) -> Times (e1,e2) 
		)
	| Div (e1,e2) -> 
		(match (freeIn e1 b ,freeIn e2 b) with
		| (true, true) -> Div (subst e1 b c,subst e2 b c)
		| (true, false) -> Div (subst e1 b c,e2)
		| (false, true) -> Div (e1,subst e2 b c)
		| (false,false) -> Div (e1,e2) 
		)
	| Lss(e1,e2) -> 
		(match (freeIn e1 b ,freeIn e2 b) with
		| (true, true) -> Lss (subst e1 b c,subst e2 b c)
		| (true, false) -> Lss (subst e1 b c,e2)
		| (false, true) -> Lss (e1,subst e2 b c)
		| (false,false) -> Lss (e1,e2) 
		)
	| Eq (e1,e2) -> 
		(match (freeIn e1 b ,freeIn e2 b) with
		| (true, true) -> Eq (subst e1 b c,subst e2 b c)
		| (true, false) -> Eq (subst e1 b c,e2)
		| (false, true) -> Eq (e1,subst e2 b c)
		| (false,false) -> Eq (e1,e2) 
		)
	| Gtr (e1,e2) -> 
		(match (freeIn e1 b ,freeIn e2 b) with
		| (true, true) -> Gtr (subst e1 b c,subst e2 b c)
		| (true, false) -> Gtr(subst e1 b c,e2)
		| (false, true) -> Gtr (e1,subst e2 b c)
		| (false,false) -> Gtr (e1,e2) 
		)
	| And (e1,e2) -> 
		(match (freeIn e1 b ,freeIn e2 b) with
		| (true, true) -> And (subst e1 b c,subst e2 b c)
		| (true, false) -> And (subst e1 b c,e2)
		| (false, true) -> And (e1,subst e2 b c)
		| (false,false) -> And (e1,e2) 
		)
	| Or (e1,e2) -> 
		(match (freeIn e1 b ,freeIn e2 b) with
		| (true, true) -> Or (subst e1 b c,subst e2 b c)
		| (true, false) -> Or (subst e1 b c,e2)
		| (false, true) -> Or (e1,subst e2 b c)
		| (false,false) -> Or (e1,e2) 
		)
	| Not(e1) -> if freeIn e1 b then Not (subst e1 b c) else Not (e1)

	| Cond(e1,e2,e3) ->(match (freeIn e1 b ,freeIn e2 b, freeIn e3 b) with
		| (true, true,true) -> Cond (subst e1 b c,subst e2 b c,subst e3 b c)
		| (true, true, false) -> Cond (subst e1 b c,subst e2 b c,e3)
		| (true, false,true) -> Cond (subst e1 b c, e2 ,subst e3 b c)
		| (true, false,false) -> Cond (subst e1 b c,e2,e3)
		| (false, true,true) -> Cond (e1,subst e2 b c,subst e3 b c)
		| (false, true, false) -> Cond (e1,subst e2 b c,e3)
		| (false, false,true) -> Cond (e1, e2 ,subst e3 b c)
		| (false, false,false) -> Cond (e1,e2,e3)
		)
	(*| Let(s1,e1,e2) -> if (s1=b) then (let e1 = Id s2 in if (s2 = b)  then let 
	 not(freeIn a b ) then a 
	else if freeIn c s then Let x = newname in Let(x, appears s1 e1 newname,appears s e2 newname) else 


let rec appears s ex name =  * Function that sees if a string appears in an expresion and then assigns the renamed name 
	match ex with
	| Id i -> if (s=i) then Id name else ex
	| Int a -> ex
	| True -> ex
	| False -> ex
	| Plus (e1,e2) -> Plus((appears s e1 name), (appears s e1 name))
	| Minus (e1,e2) -> Minus((appears s e1 name), (appears s e1 name))
	| Div (e1,e2) -> Div((appears s e1 name), (appears s e1 name))
	| Times (e1,e2) -> Times((appears s e1 name), (appears s e1 name))
	| Lss (e1,e2) -> Lss((appears s e1 name), (appears s e1 name))
	| Eq (e1,e2) -> Eq((appears s e1 name), (appears s e1 name))
	| Gtr(e1,e2) -> Gtr((appears s e1 name), (appears s e1 name))
	| And (e1,e2) -> And((appears s e1 name), (appears s e1 name))
	| Or (e1,e2) -> Or((appears s e1 name), (appears s e1 name))
	| Not(e1)-> Not(appears s e1 name)
*)
