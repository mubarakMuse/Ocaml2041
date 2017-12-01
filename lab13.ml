     (* Copyright (c) Gopalan Nadathur *)

(* Lab 13, CSci 2041, Fall 2017 *)

(* Problem 1, Code to experiment with and then comment on *)

let cond (c,t,e) =
   match c with
   | true -> t
   |  false -> e

let rec fact n =
   cond (n=0,1, n * fact (n-1))

(* Write your explanation for why this code does not work here 
It wont work because ocaml does call by value thus it will evalute the e argument all the way first and 
all the other arguments thus when it gets to fact 0 it will go to fact -1 
and then never stops becasue there is nothing that will stop the recursion since it past zero
*)

(* For Problem 2, part 1 *)

module type ITEM =
 (* the last component, initial, is meant to identify a value of item type
    that can be used in initialization *)
    sig
        type item
        val leq : item * item -> bool
        val initial : item
    end

(* Provide definitions of IntItem and StringItem here *)
module IntItem : (ITEM  with type item =int)= 
struct
    type item = int
    let leq (x, y) = if y<x then false else true
    let initial = 0
  
end

module StringItem : (ITEM  with type item = string) = 
struct
    type item = string
    let leq (x, y) = if y<x then false else true
    let initial = " "
  
end

(* For Problem 2, part 2 *)

module type HEAP =
  sig
    type item
    type tree
    exception InitHeap
    val depth : tree -> int
    val initHeap : int -> tree
    val insert : item * tree -> tree
    val isHeap : tree -> bool
    val maxHeap : tree -> item
    val replace : item * tree -> item * tree
    val size : tree -> int
    val top : tree -> item
  end


(* Using the code in lab15-heapcode.ml, define a functor called Heap that
takes a module satisfying the ITEM signature and produces a module satisfying
the HEAP signature here. *)
  module Heap (Item : ITEM): (HEAP with type item = Item.item) = 
  struct
         type item = Item.item

        let leq(p, q) : bool = Item.leq(p,q)

        let max(p,q) = if leq(p,q) then q else p
        and min(p,q) = if leq(p,q) then p else q

        let intmax((p : int),q) = if p <= q then q else p

        type tree =
          | L of item
          | N of item * tree * tree

       exception InitHeap

       let rec initHeap n =
           if (n < 1) then raise InitHeap
           else if n = 1 then L Item.initial
                else let t = initHeap(n - 1)
                     in N (Item.initial, t, t)

        let rec top t =
          match t with
          | (L i) -> i
          | N (i,_,_) -> i


        let rec isHeap t =
          match t with
          | (L _) -> true
          | (N(i,l,r)) ->
            leq(i,top l) && leq(i,top r) && isHeap l && isHeap r

        let rec depth t =
          match t with
          | (L _) -> 1
          | N(i,l,r) -> 1 + intmax(depth l,depth r)

       let rec replace (i,h) = (top h, insert(i,h))
       and insert (i, h) =
         match h with
         | L _ -> L i
         | N (_,l,r) ->
           if leq(i,min(top l,top r))
           then N(i,l,r)
           else if leq((top l),(top r))
                then N(top l,insert(i,l),r)
                else N(top r,l,insert(i,r))

       let rec size h =
         match h with
         | L _ -> 1
         | N (_,l,r) -> 1 + size l + size r

       let rec maxHeap h =
         match h with
         | (L i) -> i
         | N (_,l,r) -> max(maxHeap l, maxHeap r)
    end



(* Problem 1, part 3 *)

module IntHeap = Heap(IntItem)
module StringHeap = Heap(StringItem)