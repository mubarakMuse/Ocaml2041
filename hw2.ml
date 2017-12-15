
(*
Solution for Problem 1

1- its well typed 
let rec zip lp =
   match lp with
   | ([],_) -> []
   | (_,[]) -> []
   | ((x::l1),(y::l2)) -> (x,y) :: zip (l1,l2)

   1- intially zip is assigned to a'
   2- Based on the first match case : a' list * a' list -> a' list
   3- Based on the last match case : 'a list * 'b list -> ('a * 'b) list 

2 - Not well typed there conflic in the final match case's return value explained below
let rec reverse l =
   match l with
   | [] -> []
   | (h::t) -> (reverse t) :: h

 	1- intially zip is assigned to a'
	2- Based on the first match case : a' list  -> a' list
  	3- Based on the last match case theres a conflict becase (reverse t) return type 'a list
       but an expression was expected of type 'a The type variable 'a occurs inside 'a list
*)
(* Solution for Problem 2 *)
(* Part 1 : auxiliary function *)
let rec fib' n i temp fib1 fib2 =
	if (i<n) then fib' n (i+1) fib1 fib2 (temp+fib2)
	else fib1
(* part 2 : Invariant for Fib': the input is a 4 int tuple and 
the functions brakes the input and recursivly compute the fibonacci value with in 
the argumant tuple and return the accumalating element with in the tuple  *)

(* Part 3 :  nested auxiliary function  *)
let fib n0 = 
let rec fib'  =
	function
	|(a,b,c,d) -> if (b<a) then fib' (a,(b+1),d,(c+d))
	else c
	| _ -> 0
in fib' (n0,1,1,1)

(* Solution for Problem 3: Modified problem 8 from HW1 *)
let rec find_salary list1 str =
	match list1 with
	|[] -> None 
	|h::t -> 
		let (a,b,  c) = h in
			if ( a = str) then   Some c  
			else find_salary t str ;; 

let rec find_phno list2 str2 =
	match list2 with
	|[] -> None
	|h::t -> 
		let (a,b,c) = h in
			if (a = str2) then  Some b
			else find_phno t str2;; 
(* the option type allows us tp return None if we dont have anything 
instead of not returning anything or zero*)

(* Solution for Problem 4 *)
(* Part 1 : the invariant : for every node, all the nodes in its left subtree are smaller, 
and all of the nodes in its right subtree are larger. *)
type ('a,'b) btree =
    Empty
  | Node of 'a * 'b * ('a, 'b) btree * ('a, 'b) btree

(* Part 2 *)
let initTree = Empty
(* Part 3 *)
let tree1 = Node (10,"ten",Node (9,"nine",Empty,Empty),Node (12,"twelve",Empty,Empty))

let tree2 = Node ("dad",("x3456", 50.1),Node ("bob",("unlisted", 12.7),Empty,Empty),Node ("Max",("x1234", 107.3),Empty,Empty))

(* Part 4 : this finds the key and returns its value recursivley *)
let rec find a b = 
	match a with
	| Empty -> None
	| Node (k,d,l,r) -> if k = b then Some d
else if (k<b) then find r b
else find l b 

(* Part 5 : this inserts the item at the its right location while maintaing the BST's invaraint 
Note: if the given key already lands at indetical key it relaces the old data with the given data *)
let rec insert a b c  = 
	match a with
	| Empty -> Node (b,c,Empty,Empty)
	| Node (k,d,l,r) -> 
	if (k<b) then Node (k,d,l,insert r b c)
	else Node (k,d,insert l b c,r)

(* Part 6: Inorder traversal of the tree *)
let keylist a0 =
	
	match a0 with
	| Empty -> []
	| Node (k,d,l,r) ->  keylist l  @ [k] @ keylist r 



(* Part 7 :*)
let rec delete t i = 
	match t with
	| Empty -> t
	| Node (i',d, l, r)->  
		if (i=i') then deleteRoot t 
		else if (i<i') then Node (i',d, (delete l i), r)
		else Node (i',d, l, (delete r i))
let deleteRoot t = 
	match t with
	| Node (i',d, Empty, r)-> r
	| Node (i',d, l, Empty)-> l
	| Node (i',d, l, r)-> let (i',d',l') = largest_at_someTree l in Node(i',d',l',r')

let rec largest_at_someTree t = 
	match t with
	| Node (i,d, l, Empty)-> (i,d, l)
	| Node (i,d, l, r)-> let (i',d',r') = largest_at_someTree r in (i',d',Node(i,d,l,r'))


(* Solution for Problem 5 *)
type ty = BoolTy | IntTy | FunTy of ty * ty
type expr' =
     | Id' of string                     (* for identifiers *)
     | Int' of int                       (* for integer values *)
     | True'                             (* for the boolean value true *)
     | False'                            (* for the boolean value false *)
     | Plus' of expr' * expr'            (* for exp1 + exp2 *)
     | Minus' of expr' * expr'           (* for exp1 - exp2 *)
     | Lss' of expr' * expr'             (* for exp1 < exp2 *)
     | Gtr' of expr' * expr'             (* for exp1 > exp2 *)
     | And' of expr' * expr'             (* for exp1 && exp2 *)
     | Or'  of expr' * expr'             (* for exp1 || exp2 *)
     | Cond' of expr' * expr' * expr'    (* for if exp1 then exp2 else exp3 *)
     | Fun' of string * ty * expr'       (* for fun (x:ty) -> exp *)
     | App' of expr' * expr'             (* for (exp1 exp2) *)

(* Part 1 *)
let exp1' = Fun' ("x",IntTy,Fun'("y",BoolTy,Cond'(Id' "y", Plus' (Id' "x",Int' 1),Minus'(Id' "x",Int' 5))))
  
let exp2' = App'(Fun' ("x",IntTy, App' (Fun' ( "y", IntTy, Int' 5), Id' "x")), Int' 5)

(* Part 2 *)
let rec find01 list01 a =
	match list01 with
	| [] -> None
	| h::t -> let (a1 , b1) = h 
	in if (a1 = a) then Some b1 else find01 t a

let typeof_aux a env =
	match a with
	| Id' s -> find01 env s
	| Int' a -> Some IntTy 
	| True' -> Some BoolTy
	| False' -> Some BoolTy
	| Plus'(e1,e2) -> if (typeof_aux e1 = Some IntTy && typeof_aux e2 = Some IntTy ) then Some IntTy else None
	| Minus'(e1,e2)-> if (typeof_aux e1 = Some IntTy && typeof_aux e2 = Some IntTy) then Some IntTy else None
	| Lss'(e1, e2) -> if (typeof_aux e1 = Some IntTy && typeof_aux e2 = Some IntTy) then Some BoolTy else None
	| Gtr'(e1, e2) -> if (typeof_aux e1 = Some IntTy && typeof_aux e2 = Some IntTy ) then Some BoolTy else None
	| And'(e1, e2) -> if (typeof_aux e1 = Some BoolTy && typeof_aux e2 = Some BoolTy)  then Some BoolTy else None
	| Or'(e1, e2) -> if (typeof_aux e1 = Some BoolTy && typeof_aux e2 = Some BoolTy ) then Some BoolTy else None
	| Cond'(e1,e2,e3) -> if (typeof_aux e1 = Some BoolTy && typeof_aux e2 = typeof_aux e3 ) then Some (typeof_aux e3) else None
	| Fun'(e1,e2,e3) -> match typeof_aux e3 ((e1, e2)::env) with
						(
							| None -> None
							| Some t -> Some FunTy(e2, t)
						)
	| App' (e1, e2) -> 
		(
			match typeof_aux e1 env, typeof_aux e2 env with
			| Some FunTy(e11, e12), Some e21 -> if (e11 = e21) then Some e21 else None
			| _ -> None 
		)




(* Solution for Problem 6 *)

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
(* part 1*)



let rec eval express list1 = 
	match express with
	| Id e1 ->  Some e1 (*find01 on e1*)
	| Int e1 -> Some e1
	| True -> Some true
	| False -> Some false
	| Plus (e1, e2) -> 
		(match (e1, e2) value with
		| Int a,Int b -> a + b
		| Id a, Id b -> find01 list1 a + find01 list1 b
		| Id a,Int b -> find01 list1 a + b
		| Int a,Id b -> a + find01 list1 b )

	| Minus (e1, e2) ->
		(match (e1, e2) value with
		| Int a,Int b -> a - b
		| Id a, Id b -> find01 list1 a - find01 list1 b
		| Id a,Int b -> find01 list1 a - b
		| Int a,Id b -> a - find01 list1 b )

	| Times(e1, e2) -> 
		(match (e1, e2) value with
		| Int a,Int b -> a * b
		| Id a, Id b -> find01 list1 a * find01 list1 b
		| Id a,Int b -> find01 list1 a * b
		| Int a,Id b -> a * find01 list1 b )

	| Div (e1, e2) -> 
		(match (e1, e2) value with
		| Int a,Int b -> a/b
		| Id a, Id b -> find01 list1 a / find01 list1 b
		| Id a,Int b -> find01 list1 a / b
		| Int a,Id b -> a / find01 list1 b )


	| Lss (e1, e2) -> 
		(match (e1, e2) value with
		| Int a,Int b -> if a<b then true else false
		| Id a, Id b -> if ((find01 list1 a) < (find01 list1 b)) then true else false
		| Id a,Int b -> if ((find01 list1 a) < b) then true else false
		| Int a,Id b -> if (a < (find01 list1 b )) then true else false)
		

	| Eq (e1, e2) -> 
		(match (e1, e2) value with
		| Int a,Int b -> if a=b then true else false
		| Id a, Id b -> if ((find01 list1 a) < (find01 list1 b)) then true else false
		| Id a,Int b -> if ((find01 list1 a) < b) then true else false
		| Int a,Id b -> if (a < (find01 list1 b )) then true else false)

	| Gtr (e1, e2) ->
		(match (e1, e2) value with
		| Int a,Int b -> if a=b then true else false
		| Id a, Id b -> if ((find01 list1 a) < (find01 list1 b)) then true else false
		| Id a,Int b -> if ((find01 list1 a) < b) then true else false
		| Int a,Id b -> if (a < (find01 list1 b )) then true else false)

	
	| And (e1, e2) ->
	| Or (e1, e2) ->
	| Not (e1, e2) ->
	| Cond (e1, e2, e3) ->
	| Let (e1, e2, e3) -> eval e3 ((e1,e2)::list1)
*)
