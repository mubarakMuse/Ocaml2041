                 (* Copyright (c) Gopalan Nadathur *)

(* A type for mutable lists; lists are references to list cells that
   either indicate an empty list or a head element and a tail list *)
type 'a mylist = 'a listcell ref
  and  'a listcell = Nil | Cons of 'a * ('a mylist)


(* Solution to Problem 1
    append : 'a mylist -> 'a mylist -> 'a mylist *)

  (* This is just a stub to make sure the code compiles.
     You should fill in a definition for the function below
     as per the problem description
  *)
  let rec append l1 l2 = 
  match !l1 with 
  | Nil -> (l1:= !l2; l1)
  | Cons (a,b)-> ref (Cons(a,append b l2))

(*
 * Problem 1.1
 * Score: 1.5/3
 * Comments: You are just constructing a new list instead of updating l1
 *)

(* Solution to Problem 2
    rev_app : 'a mylist -> 'a mylist -> 'a mylist *)

(* let rec rev_app l1 l2 =
   Again, what is given to you is just a stub. You should
      replace it with a correct definition for the function.
  *)
   let rec rev_app l1 l2 = 
    let rec reverse l acc = 
     match  !l with
      | Nil ->  acc
      | Cons (a,b) -> let nextL = b in (l := Cons (a,acc)  ; reverse (nextL) (l))
      in  append (reverse l1 (ref Nil)) (l2) 
(* 3/3 *)

