(*
Solution to Problem 1
1. well typed and value is 4
2. not well typed because it use the plus operator but it adding a float to and int
3. not well typed becaue the 7 has to be a float
4.  well typed and value is 7
5.  not well typed it need the type has the be the same after the then and else.
6. not well typed because its missing some of the if structure ( else part)
(* if the else is omittted, it is implicitly taken to be () and then int and unit don't match
 *)
7. well typed and value is the string hello world
8. well tyoe and the fuction (string -> string) * string = (<fun>, "hello")
(* 7.5/8 *)

Solution to Problem 2
1. Illegal because of unbound value of y (not assigned yet)
2. legal value : int 3
3. illegal unbound of x
4. legal value : int 3
5. legal value : int 5

  Problem 2 Score: 5/5

*)



let rec gcd a b =
	if a = b then a
 	else
  		if a > b then gcd (a-b) b
  		else gcd a (b-a)

let reduced_form tuple1 =
  match tuple1 with
  |(x,y) -> if gcd x y = 1
    then (x,y)
    else ((x/(gcd x y)),(y/(gcd x y)))
  |(_,_) ->  (0,0);;

	(* SCORE (Prob 4): 4.5/5
	* -0.5 : Missing comments (include type, preconditions and invariant)
	*)

(*No comment with types, precond, or invariant*)
let rec fromMtoN m n =
  if m>n
    then []
  else m:: fromMtoN (m+1) n;;
(*4.5/5*)

(* Score: 3.5/5 -- Needs comments, this won't run but you have the correct idea,
 * you'll need to change the left hand side of the match cases (don't use square
 * brackets, this implies a list). Ex:
 * | [] ->
 * | h::l::t -> l :: everyEven t
 * *)
let rec everyEven list1 =
  match list1 with
  |[] -> []
  |[h::l::t] -> l :: everyEven t
  |[h::t]-> t ;;


let rec everyNth list1 n =
	let count = 1
	let help list2 count =
	match list2 with
	| [] -> []
	| [h::t] -> if (counter mod n = 0)
					then h:: help t count+1;;

(*
	-5: Syntax error. Your function cannot run.
  Problem 7 Score: 0/5
 *)


let rec find_salary list1 str =
	match list1 with
	|[] -> 0
	|[h::t] -> (a,b,c) = h
	if (a = str) then -> c
	else find_salary t str ;;

let rec find_phno list2 str2 =
	match list2 with
	|[] -> 0
	|[h::t] -> (a,b,c) = h
	if (a = str) then -> b
	else find_phno t str;;

(* Score: 2/8
 * Missing comments.
 * Fails to parse/run.
 *)

(* No is_matrix 0/5 *)
(* No matrix_scalar_multiply 0/3 *)
