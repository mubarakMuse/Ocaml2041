(* Solution for Problem 1 *)


(* Problem 1, Part 1 Score:
 * makePairList: /3
  * makeAllPairLists: /3
   * TOTAL: 5.5/6
    * Comments:  Write documentation
     *)


 let makePairLists p fl = List.map (fun x -> if x<p then ((x,p),fl) else ((p,x),fl)) fl

 let makeAllPairLists l =  List.map (fun x -> let (s,l1) = x in makePairLists s l1) l

 let friendsList =
   [ ("a", ["b"; "c"; "d"]);
     ("b", ["a"; "c"; "d"; "e"]);
     ("c", ["a"; "b"; "d"; "e"]);
     ("d", ["a"; "b"; "c"; "e"]);
     ("e", ["b"; "c"; "d"]) ]

(* HELPER *)
let rec appears a list1 =
  match list1 with
  | [] -> false
  | h::t-> if h=a then true else appears a t

let intersect l1 l2 = List.fold_right (fun x y -> if (appears x l2) then x::y else y) l1 []

let rec addOnePair ((p1,p2),lp) l1 =
  		match  l1 with
  		|[]-> ((p1,p2),lp)::[]
  		| ((x1,x2),lx)::t-> if( x1 = p1 && x2 = p2) then ((p1,p2),intersect lp lx)::t
	  			else ((x1,x2),lx)::addOnePair ((p1,p2),lp) t

let addAllPairs ppls l = List.fold_left (fun x y -> addOnePair y x ) l ppls

(* Problem 1, Part 2 Score:
 * intersect: 3/3
 * addOnePair: 4/4
 * addAllPairs: 3/3
 * TOTAL: 9/10
 * Comments:
 -1: loss of documentation
 *)


let commonFriends l = List.fold_right addAllPairs (makeAllPairLists l) []

(* Solution for Problem 2 *)
type 'a olist = { data : 'a list ; ord : 'a -> 'a -> bool;}

let initOList ord = { data = [] ; ord = ord;}

let list1 = { data = [1; 5; 7; 12; 13]; ord = (fun x y -> if x<y then true else false);}
let list2 = { data = [17; 14; 13; 9; 2]; ord = (fun x y -> if x>y then true else false);}
let list3 ={ data = [17; 19; 6; 13; 2]; ord = (fun x y -> if x>y then true else false);}

(* Problem 2, Part 1,2, & 3 Score:
 * olist type: 3/3
 * initOList: 2/2
 * list1|2|3 defs: 3/3
 * TOTAL: 7.5/8
 * Comments: -0.5 no comments/documentation
 *)

let rec isOrderedList {data = l; ord = ord}=
	match l with
	| [] -> true
	| _::[]->true
	| h1::h2::t-> (ord h1 h2) && isOrderedList {data = h2::t; ord = ord}

	(* HELPER *)
let rec insert_Help f l1 a=
	match l1 with
	| [] -> [a]
	| h::t-> if (f a h) then a::h::t else h::insert_Help f t a

let insertOList a {data = l; ord = ord} =
	match l with
	| [] -> {data = [a]; ord = ord}
	| _ -> {data = (insert_Help ord l a); ord =ord}

let olistToList {data = l; ord = ord} =
	match l with
	| []-> []
	| _ -> l
(* Problem 2, Part 4,5, & 6 Score:
 * isOrderedList: 3/3
 * insertOList: 3/3
 * olistToList: 2/2
 * TOTAL: 8/8
 * Comments:
 *)
(* Solution for Problem 3 *)
let rec append l1 l2 =
     match l1 with
     | [] -> l2
     | (h::t) -> h::append t l2

let rec cont_append l1 l2 c =
	match l1 with
  | [] -> (c l2)
  | h::t -> cont_append t l2 (fun x -> c(h::x))

(* Problem 3, Part 1 Score:
 * TOTAL: 3.75/4
 * Comments: Missig comments
 *)

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let rec sumTree t =
     match t with
     | Empty -> 0
     | Node (i,l,r) -> i + sumTree l + sumTree r

let rec cont_sumTree t c =
   match t with
   | Empty -> (c 0)
   | Node (i,l,r) -> cont_sumTree r (fun y -> c (cont_sumTree l (fun x -> x+y+i)))

(* Problem 3, Part 2 Score:
 * TOTAL: 4.5/6
 * Comments:
 * - Missing comments.
 * - In recursive case c should be called from inner continuation. Not
 *   tail-recursive as is.
 *)
