type state = int * int * int * int *int 
let getI (i,fib1,fib2,temp,n) = i
let getFib1 (i,fib1,fib2,temp,n) = fib1
let getFib2 (i,fib1,fib2,temp,n) = fib2
let getTemp (i,fib1,fib2,temp,n) = temp
let getN (i,fib1,fib2,temp,n) = n

let putI exp s =
   let (i,fib1,fib2,temp,n) = s in (exp s,fib1,fib2,temp,n)
let putFib1 exp s =
   let (i,fib1,fib2,temp,n) = s in (i,exp s,fib2,temp,n)
let putFib2 exp s =
   let (i,fib1,fib2,temp,n) = s in (i,fib1,exp s,temp,n)
let putTemp exp s =
   let (i,fib1,fib2,temp,n) = s in (i,fib1,fib2,exp s,n)
let putN exp s =
   let (i,fib1,fib2,temp,n) = s in (i,fib1,fib2,temp,exp s)


let seq stat1 stat2 =
  	 fun s -> (stat2 (stat1 s))

let ifstat exp stat1 stat2 =
    fun s -> if (exp s) then (stat1 s)
             else (stat2 s)

let rec whilestat exp stat =
   fun s ->
      ifstat exp
             (seq stat (whilestat exp stat))
             (fun x -> x) s

let one = fun s -> 1

let i_plus_one =
      (fun s -> (getI s) + 1)
let i_notEqualto_y =
      (fun s -> not((getI s) == (getN s)))
let to_fib1 =
      (fun s -> (getFib1 s))
let to_fib2 =
      (fun s -> (getFib2 s))
let to_fib2_Plus_Temp =
      (fun s -> (getFib2 s) + (getTemp s))
  


let fibprog = 
        (seq (putI one) 
        	(seq (putFib1 one)
        		(seq (putFib2 one)
                   (whilestat i_notEqualto_y
                      (seq (putTemp to_fib1) 
                           (seq (putFib1 to_fib2)
                       (seq (putFib2 to_fib2_Plus_Temp)
                   (putI i_plus_one))
               ))))))

let fib7 () = getFib1 (fibprog (0,0,0,0,7))
let fib a = getFib1 (fibprog (0,0,0,0,a))

(*

 
 (* Copyright (c) Gopalan Nadathur *)

(* Problem 2, First Part *)

let rec sumup n =
    if (n = 0) then 0
    else n + sumup (n-1)


(* 
Part 1: P(n) = sumup n  is the sum of the natural numbers from 0 to n
Part 2: 
   P(0) is the statement "sumup 0 is the sum of the natural numbers from 
                          0 to 0."
     This part is obvious from the definition of sumup
  P(n+1): We assume P(n) is true. This means that sumup n is the sum 
     of the numbers from 0 to n. But then sumup (n+1) adds n+1 to 
     sumup n and this must therefore be equal to the sum of the numbers 
     from 0 to (n+1). Thus P(n+1) is true.
*)

(* Problem 2, Second Part *)

  let sumup' n =
    let rec sumup_aux n acc =
        if (n = 0) then acc
        else sumup_aux (n-1) (n + acc)
    in sumup_aux n 0

(* Proof
Part 1: 
  sumup_aux n acc is the sum of the natural numbers from 0 to n added to acc
Part 2: 
  We need to prove 
   P(0): sumup_aux 0 acc is the sum of the natural numbers from 0 to 0 
         added to acc. The latter is acc and this is in fact what 
         sumup_aux 0 acc evaluates to.
   P(n+1): The induction hypothesis gives us that sumup_aux n ((n+1) + acc)
         is the sum of the natural numbers from 0 to n added to (n+1) + acc).
         But this is the same as the sum of the natural numbers from 0 to
         (n+1) added to acc. But then P(n+1) is true because 
         (sumup_aux (n+1) aux) evaluates to (sumup_aux n ((n+1) + acc)).
Part 3: (sumup' n) evaluates to (sumup_aux n 0) which, by Part 2, evaluates
        to the sum of the natural numbers from 0 to n added to 0, i.e. the 
        sum of the natural numbers from 0 to n. 
*)


(* Problem 3 *)

(* The type and functions definitions *)
 type nat = Zero | Succ of nat

let rec plusNat x y =
   match x with
   | Zero -> y
   | (Succ x') -> Succ (plusNat x' y)

let rec multNat x y =
   match x with
   | Zero -> Zero
   | (Succ x') -> plusNat y (multNat x' y)

let rec toInt n =
   match n with
   | Zero -> 0
   | (Succ n') -> toInt n' + 1

(* 
A known fact
  for all n1 in nat,
    for all n2 in nat, toInt (plusNat n1 n2) = (toInt n1) + (toInt n2)
We need to prove the following:
  for all n1 in nat,
    for all n2 in nat, toInt (multNat n1 n2) = (toInt n1) * (toInt n2)
We will prove this by induction on n1. This means that we need to
prove the following:
(1) forall n2 in nat, toInt (multNat Zero n2) = (toInt Zero) * (toInt n2)
On the left, we observe that (toInt (multNat Zero n2)) evaluates to
(toInt Zero), which evaluates to 0. On the right, one step in the
evaluation takes us to 0 * (toInt n2). We can now show by induction
on n2 that (toInt n2) must terminate. Then the right hand side must
also evaluate to 0.
(2) Assuming that the following holds
    for all n2 in nat, toInt (multNat n1 n2) = (toInt n1) * (toInt n2) (1)
we must show
    for all n2 in nat, toInt (multNat (Succ n1) n2) =
                 (toInt (Succ n1)) * (toInt n2)
Let us then pick an arbitrary n2 and show the following
   toInt (multNat (Succ n1) n2) = (toInt (Succ n1)) * (toInt n2)
Taking one step in the evaluation of the left, we see that it will
evaluate to whatever
   toInt (plusNat n2 (multNat n1 n2))
evaluates to. Using the known fact about plusNat, we see that this
expression evaluates to whatever
   toInt n2 + toInt (multNat n1 n2)
evaluates to. Using the induction hypothesis noted above, we see that
this expression will evaluate to whatever
   toInt n2 + (toInt n1) * (toInt n2)     (2)
evaluates to.
Now lets work with the righthand side of the equality to be
shown. Taking one step in its evaluation, we see that it evaluates to
whatever
   ((toInt n1) + 1) * toInt n2
evaluates to. It is easy to see that the latter expression can be
simplified to
   (toInt n1) * (toInt n2) + toInt n2     (3)
But now it is clear that (2) and (3) have the same termination
behaviour and that they also must have the same values if they
terminate.
*)

(* Problem 4 *)

(* First, let us reproduce the function definitions: *)

let rec append l1 l2 =
   match l1 with
   | [] -> l2
   | (h::t) -> h :: (append t l2)

let rec reverse l =
   match l with
   | [] -> []
   | (h::t) -> append (reverse t) [h]

let rec sumlist lst =
   match lst with
   | [] -> 0
   | (h::t) -> h + (sumlist t)

(*
We also note that we have proved the following already:
  for all integer lists l1,
    forall lists l2, sumlist (append l1 l2) = (sumlist l1) + (sumlist l2)
We now want to prove the following:
  for all integer lists l, sumlist (reverse l) = sumlist l
We will use induction on l. Thus we are reduced to showing the
following:
(1) sumlist (reverse []) = sumlist []. This is trivial.
(2) Assuming that
       sumlist (reverse l) = sumlist l
we need to show that
       sumlist (reverse (h::l)) = sumlist (h::l)
Using the definition of reverse, the lefthand side evaluates to
whatever
       sumlist (append (reverse l) [h])
evaluates to. Using the known fact about append and the definition of
sumlist, we see that this must evaluate to whatever
       sumlist (reverse l)  + h
evaluates to. Using the induction hypothesis, we see that this
evaluates to whatever
       sumlist l + h      (1)
evaluates to. Working with the righthand side of the equation and
using the definition of sumlist, we see that that must evaluate to
whatever
       h + sumlist l    (2)
evaluates to. (1) and (2) have the same termination behaviour and
also the same value if they terminate, so we have established the
desired conclusion.
*)
   
(* Problem 5 *)

(* Again, let us reproduce the type and function definitions first: *)

type 'a btree =
  | Empty
  | Node of 'a * 'a btree * 'a btree

let rec maxTree =
   function
     | Empty -> None
     | Node (i,l,r) ->
         match (maxTree r) with
         | None -> (Some i)
         | (Some i') as m -> m

let rec minTree =
   function
     | Empty -> None
     | Node (i,l,r) ->
         match (minTree l) with
         | None -> (Some i)
         | (Some i') as m -> m

let rec insert t i =
   match t with
   | Empty -> Node (i,Empty,Empty)
   | Node (i',l,r) ->
        if (i < i') then Node (i',insert l i,r)
        else Node (i',l,insert r i)

(*
Now we consider the proofs of each of the properties in turn.
I. Here we prove
   for all t of type ('a btree),
     for all x of type 'a, (if (insert t x) evaluates to t' then either
                             minTree t' = minTree t or minTree t' = Some x)
Proof: By induction on t
   t is Empty:
    In this case, we have to show that
     for all x of type 'a, (if (insert Empty x) evaluates to t' then either
                             minTree t' = minTree t or minTree t' = Some x)
    We assume x to be arbitrary and show that the property holds.
    Observing that t' = Node (x,Empty,Empty), is easy to check that
     minTree t' is Some x.
   t is Node (i,l,r):
     Once again, we assume x to be arbitrary and show that
      if (insert t x) evaluates to t' then either
           minTree t' = minTree t or minTree t' = Some x)
     assuming that
      if (insert l x) evaluates to l' then either
           minTree l' = minTree l or minTree l' = Some x)
     We have two cases to consider:
        (x < i). Here t' = Node (i,l',r) where l' = insert l x. By induction
             either (minTree l' = minTree l) or minTree l' = Some x.
             In the former case, it is easy to see that minTree t' = minTree t.
             In the latter case, minTree t' = Some x. In either case, we have
             the required property.
       (x is not less than i). Here t' = Node (i,l,insert r). In this case,
             minTree t and minTree t' depend in the same way on minTree l and
             i and hence must be equal.
II. Here we have to prove
   for all t of type ('a btree),
     for all x of type 'a, (if (insert t x) evaluates to t' then either
                             maxTree t' = maxTree t or maxTree t' = Some x)
Proof: By induction on t; the proof is symmetric to the one in part I,
but is nevertheless reproduced below.
   t is Empty:
    In this case, we have to show that
     for all x of type 'a, (if (insert Empty x) evaluates to t' then either
                             maxTree t' = maxTree t or maxTree t' = Some x)
    We assume x to be arbitrary and show that the property holds.
    Observing that t' = Node (x,Empty,Empty), is easy to check that
     maxTree t' is Some x.
   t is Node (i,l,r):
     Once again, we assume x to be arbitrary and show that
      if (insert t x) evaluates to t' then either
           maxTree t' = maxTree t or maxTree t' = Some x)
     assuming that
      if (insert r x = r') then either
           maxTree r' = maxTree r or maxTree r' = Some x).
     We have two cases to consider:
        (x < i). Here t' = Node (i,insert l x,r). In this case,
             (maxTree t) and (maxTree t') depend in the same way on
             (maxTree r) and i and hence must be equal.
       (x is not less than i). Here t' = Node (i,l,r') where r' =
             insert r x. By induction, either (maxTree r' = maxTree r)
             or maxTree l' = Some x. In the former case, it is easy to
             see that maxTree t' = maxTree t. In the latter case,
             maxTree t' = Some x. In either case, we have the required
             property.
*)
*)