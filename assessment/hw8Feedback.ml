
(* problem 1  *)
(* part a

(* Problem 1, Part 1 Score:
 * Part 1(a): 1/2
 * Part 1(b): 1/1
 * Part 1(c): 1/2
 * TOTAL: 3j/5
 * Comments: Prob size shrinks by 2 at each iteration
 *)

the cost of making and returning a list as c1
the cost of a recursive call as c2
the cost of the return from a recursive call as c3
the cost of the saving list in a a recursive call as c4
T(n) = ( 	c1                            l = []
			c1						      l = [_]

			T(n − 1) + c1 + c2 + c3 +c4    l = [h1::t1,h2::t2]

part b
T(n) = T(n − 1) + c1 + c2 + c3 + c4

T(n) = c1 + n ∗ (c4 + c2 + c3) *)

(* Problem 1, Part 2 Score:
 * Part 2(a): 0/2
 * Part 2(b): 0/2
 * TOTAL: 0/4
 * Comments: I didn't see the answer for this question
 *)

 (* SCORE Problem 1, Part 3 (a) and (b):   0/4
 *   Part 3a:   0/2
 *   Part 3b:   0/2
 * couldnt find an answer?
 *)
 (* Problem 1, Part 3 (c) (d) Score:
  * Part 3(c): 0/1
  * Part 3(d): 0/1
  *)
(* problem 2  *)
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

(* Problem 2 Score:
 * is_RBTree_aux: 3/5
 * is_RBTree: 1/1
 * bh_RBTree: 1/1
 * TOTAL: 5/7
 * Comments:
 - Incorrect calculation of black height, counts all black nodes.
 - is_RBTree should ia possible (and much simpler) as a single function.
 * true && X is the same as just X.
 *)
