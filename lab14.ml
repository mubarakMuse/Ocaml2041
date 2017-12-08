                (* Copyright (c) Gopalan Nadathur *)


(* Problem 1 *)

(* The code that you have to consider in this problem *)

  let rec append l1 l2 =
     match l1 with
     | [] -> l2
     | (h::t) -> h::(append t l2)

  let head l =
     match l with
     | [] -> 0
     | (h::t) -> h

(* The expression whose evaluation you have to consider:
       head (append (append [1;2] [3]) [4])
Part 1: Show the steps in call-by-name evaluation below
Part 2: Show the steps in call-by-value evaluation below
*)

(* Problem 2 *)

(* The code given to you at the outset *)

type 'a stream = Stream of (unit -> 'a * 'a stream)

let mkStream f = Stream f
let nextStream (Stream f) = f ()

let rec fromNStream n = mkStream (fun () -> (n, fromNStream (n+1)))
let natStream = (fromNStream 1)

(* Write the definition of zipStreams below *)


(* Problem 3 *)

(* The code given to you to analyze *)
let fib n =
   let rec fib' n i fib1 fib2 =
      if (n = i) then fib1
      else fib' n (i+1) fib2 (fib1+fib2)
   in fib' n 1 1 1

(* Part 1
   Replace this text with the recurrence equation for the running time for
   (fib' n i f s) expressed as a function of (n-i).
*)

(* Part 2
   Replace these sentences with your guess of a solution for the recurrence
   relation. Include in what you write a rationale underlying the guess.
*)

(* Part 3
   Replace this sentence with your answer to this part.
*)
   

(* Problem 4 *)

(* The two functions to be analyzed *)
let rec exp m =
   function
     | 0 -> 1
     | n ->  m * exp m (n-1)


let rec exp' m =
  function
  | 0 -> 1
  | 1 -> m
  | n ->
    let b = exp' m (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else m)

(*
  Replace this sentence with your answers to the different parts of this
  problem.
*)