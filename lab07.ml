let rec map2 f l u =
  match(l,u) with
  |([],[]) -> []
  |(h::t,[]) -> []
  |([],h::t) -> []
  |(h1::t1,h2::t2) -> f h1 h2:: map2 f t1 t2

let rec accumulate f lst u =
  match lst with
  | [] -> u
  | (h::t) -> accumulate f t (f h u)

let rec reduce f lst u =
  match lst with
  | [] -> u
  | (h::t) -> f h (reduce f t u)
    
let rec appears a list1 = 
  match list1 with
  | [] -> false
  | h::t-> if h=a then true else appears a t

let union l1 l2 = accumulate (fun x y -> if (appears x y) then y else x::y) l1 l2

let  unzip l = reduce (fun (x,y) (l1,l2)-> (x::l1,y::l2) ) l ([],[])

let zip l1 l2 = map2 (fun x y -> (x,y))  l1 l2



(*
Compose: 
1- 'a->'b->'c (before = )
2- 'a->'b->'d->'c (from fun  x)
3- 'a->('d->'e)->'d->'c (form (g x))
4- ('e -> f')->('d->'e)->'d->'f (From (f (g x)))
5- ('a->'b)-> ('c->'a) -> 'c -> 'b (rewirte it )

compose (fun x -> x+x) (fun x -> x + " 7") 10 //  doesnt work
compose (fun x -> x+x) (fun x -> x + 7) 10 // works

forsome: 
1- 'a -> 'b -> 'c
2- 'a -> 'b list -> bool
3- ('b -> bool) -> 'b list -> bool


*)