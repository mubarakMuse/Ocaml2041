type 'a stream = Stream of (unit -> 'a * 'a stream)

let mkStream f = Stream f
let nextStream (Stream f) = f ()
let rec fromNStream n = mkStream (fun () -> (n, fromNStream (n+1)))

let natStream = (fromNStream 1)
let rec mapStream f s =
	let x,rest = nextStream s in mkStream (fun () -> (f x, mapStream f rest)) 
let rec helper s c  = 
	let (x1,s1) = nextStream s in 
		let (x2,c1) = nextStream c in 
			if (x1 == x2) then mkStream (fun() -> (x1, helper s1 c1))
		else if x1>x2 then helper s c1 else helper s1 c
				
let squareStream = (mapStream (fun x -> x*x) natStream);;
let cubeStream = (mapStream (fun x -> x*x*x) natStream);;
let squarecubeStream = helper squareStream cubeStream

type 'a stream' = Stream' of 'a stream_aux ref
and  'a stream_aux =
       | Evald of ('a * 'a stream')
       | UnEvald of (unit -> 'a * 'a stream')
let x = ref 1
let mkStream' f = let x = ref (UnEvald f) in Stream' x
let rec nextStream' (Stream' x) = 
	match !x with 
	| Evald e1 -> e1
    | UnEvald e1 -> let (x,rest) = e1() in let r = ref (Evald (x,rest)) in nextStream' (Stream' r)
 let rec fromNStream' n =  mkStream' (fun ()-> (n,fromNStream' (n+1)))
 let natStream' = fromNStream' 1
