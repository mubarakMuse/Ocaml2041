(*
Problem 1 

let rec map f l =
   match l with
   | [] -> []
   | (h::t) -> (f h) :: map f t

let sqr n = n * n

let rec take l n =
  match (l,n) with
  | ((_,0) | ([],_)) -> []
  | (h::t,n) -> h :: (take t (n-1))

Part 1- Call by name:

take (map sqr [1;2;3]) 2
take (sqr 1 :: map sqr [2;3]) 2
take (1*1 :: map sqr [2;3]) 2
take (1 :: map sqr [2;3]) 2
1 :: take (map sqr [2;3]) 1
1 :: take (map sqr [2;3]) 1
1 :: take (sqr 2 :: map sqr [3]) 1
1 :: take (2*2 :: map sqr [3]) 1
1 :: take (4 :: map sqr [3]) 1
1 :: 4 :: take (map sqr [3]) 0
1 :: 4 :: []

Part 2 - Call by value:

take (map sqr [1;2;3]) 2
take (sqr 1 :: map sqr [2;3]) 2
take (1*1 :: map sqr [2;3]) 2
take (1 :: map sqr [2;3]) 2
take (1 :: sqr 2 :: map sqr [3]) 2
take (1 :: 2*2 :: map sqr [3]) 2
take (1 :: 4 :: map sqr [3]) 2
take (1 :: 4 :: sqr 3 map sqr []) 2
take (1 :: 4 :: 3*3 map sqr []) 2
take (1 :: 4 :: 9 map sqr []) 2
take (1 :: 4 :: 9 :: []) 2
take [1;4;9] 2
1 :: take [4;9] 1
1 :: 4 :: take[9] 0
1 :: 4 :: []
[1;4]

*)
(* Problem 2 *)
type 'a stream = Stream of (unit -> 'a * 'a stream)

let mkStream f = Stream f
let nextStream (Stream f) = f ()
let rec fromNStream n = mkStream (fun () -> (n, fromNStream (n+1)))

let natStream = (fromNStream 1)
(*Part 1*)
let rec mapStream f s =
	let x,rest = nextStream s in mkStream (fun () -> (f x, mapStream f rest)) 

(*Part 2 *)				
let squareStream = (mapStream (fun x -> x*x) natStream);; (* mapping using the def of square (x*x) *)
let cubeStream = (mapStream (fun x -> x*x*x) natStream);; (* mapping using the def of cube (x*x*x) *)

(*Part 3*)
let rec helper s c  = (* helper to find where the it both the cubed and sqaured and moves foward accodingly*)
	let (x1,s1) = nextStream s in 
		let (x2,c1) = nextStream c in 
			if (x1 == x2) then mkStream (fun() -> (x1, helper s1 c1))
		else if x1>x2 then helper s c1 else helper s1 c

let squarecubeStream = helper squareStream cubeStream

(*Problem 3*)

type 'a stream' = Stream' of 'a stream_aux ref
and  'a stream_aux =
       | Evald of ('a * 'a stream')
       | UnEvald of (unit -> 'a * 'a stream')
(*Part 1*)
let mkStream' f = let x = ref (UnEvald f) in Stream' x

let rec nextStream' (Stream' f) = 
	match !f with 
	| Evald e1 -> e1
    | UnEvald e1 -> let (x,rest) = e1() in ( f:= Evald(x,rest); nextStream' (Stream' f)) (* update my refrence value with itsi self but in the Evald type and then evaluting it*)
(*Part 2*)
let rec fromNStream' n =  mkStream' (fun ()-> (n,fromNStream' (n+1)))
let natStream' = fromNStream' 1


