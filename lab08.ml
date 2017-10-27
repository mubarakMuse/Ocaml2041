(* Solution for Problem 1 *)
let rec processWord l1 s =
	match l1 with
	| [] -> [(s,1)]
	| (h1,h2)::t -> if (h1=s) then (s,(h2+1))::t else (h1,h2)::processWord t s

let processOneList l = List.fold_left processWord [] l

let rec assimilateWordCount l1 (t1,t2) =
	match l1 with
	| [] -> [(t1,t2)]
	| (h1,h2)::t -> if (t1 = h1) then ((h1,(h2+t2))::t) else if (h1>t1) then ((t1,t2)::(h1,h2)::t) else ((h1,h2)::(assimilateWordCount t (t1,t2)) )

let assimilateWCList wcs wcl =
        List.fold_left assimilateWordCount wcs wcl

let wordCounts ls =
      List.fold_left assimilateWCList [] (List.map processOneList ls)

(* Solution for Problem 2 *)

 type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

(* HELPER *)
let rec insert f x t  =
	match t with
   	| Empty -> Node (x,Empty,Empty)
   	| Node (x',l,r) ->
        if (f x x') then Node (x',insert f x l,r)
        else Node (x',l,insert f x r)

type 'a bstree = { data : 'a btree ; lss : 'a -> 'a -> bool;}

let insert_bstree {data = t; lss = lss;} i =
		match t with
		| Empty -> {data = Node(i,Empty,Empty); lss = lss;}
		| Node (ind,l,r) -> {data = (insert lss i t); lss = lss;}

(* Solution for Problem 3 *)

let rec cont_fact n c =
	match n with
  | 0 -> (c 1)
  | _ -> cont_fact (n-1) (fun x -> c (n * x))

let rec tr_fact n acc =
   if (n = 0) then acc
   else (tr_fact (n-1) (n*acc))

(*  tr_fact is tail recursive it goes like 1*2*3*4*5*1* 
while cont_fact goes like 5*4*3*2*1*1
*)