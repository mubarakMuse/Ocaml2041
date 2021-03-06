
type color = R | B

type 'a rbtree =
    Empty
  | Node of color * 'a * 'a rbtree * 'a rbtree

(* part 1 *)
let rec children t = (* check the first property of rbtree *)
	match t with
	| Empty -> true
	| Node(c,i,l,r) -> if (c == R) then (match (l,r) with
											| Empty,Empty -> true 
											| Node(c1,i1,l1,r1),Empty -> (c1==B) && children l && children r
											| Empty,Node(c1,i1,l1,r1) -> (c1==B) && children l && children r
											| Node(c1,i1,l1,r1),Node(c2,i2,l2,r2) -> (c1==B) && (c2==B) && children l && children r
										)
else children r && children l


 let rec bHieght t = (* calculates the Black hieght of the tree *)
	match t with
	| Empty -> 0
	| Node(c,i,l,r) -> if (c == B) then (match (l,r) with
											| Empty,Empty -> 0
											| Node(c1,i1,l1,r1),Empty -> if (c1==B) then 2 + bHieght l + bHieght r else bHieght l + bHieght r
											| Empty,Node(c1,i1,l1,r1) -> if (c1==B) then 2 + bHieght l + bHieght r else bHieght l + bHieght r
											| Node(c1,i1,l1,r1),Node(c2,i2,l2,r2) -> if ((c1==B) && (c2==B)) then 2 + bHieght l + bHieght r else bHieght l + bHieght r
										)
											else bHieght l + bHieght r


let rec blackcount t = (* checks the second property of rbtrees *)
	match t with
	| Empty -> true
	| Node(c,i,l,r) -> (match (l,r) with
								| Empty,Empty -> true 
								| Node(c1,i1,l1,r1),Empty -> if (c1==B) then (true && blackcount l && blackcount r) else true && blackcount l && blackcount r
								| Empty,Node(c1,i1,l1,r1) ->  if (c1==B) then (true && blackcount l && blackcount r) else true && blackcount l && blackcount r									
								| Node(c1,i1,l1,r1),Node(c2,i2,l2,r2) -> if (c1==B && c2==B) then true && blackcount l && blackcount r else true && blackcount l && blackcount r
										)




let is_RBTree_aux t = (bHieght t, children t && blackcount t)

(* part 2 *)
let is_RBTree t = let (n,b) = is_RBTree_aux t in  b

(* part 3 *)
let bh_RBTree t = let (n,b) = is_RBTree_aux t in if b then Some n else None

