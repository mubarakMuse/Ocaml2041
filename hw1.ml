(* 
Solution to Problem 1
1. well typed and value is 4
2. not well typed because it use the plus operator but it adding a float to and int
3. not well typed becaue the 7 has to be a float
4.  well typed and value is 7
5.  not well typed it need the type has the be the same after the then and else.
6. not well typed because its missing some of the if structure ( else part)
7. well typed and value is the string hello world
8. well tyoe and the fuction (string -> string) * string = (<fun>, "hello")

Solution to Problem 2
1. Illegal because of unbound value of y (not assigned yet)
2. legal value : int 3
3. illegal unbound of x
4. legal value : int 3
5. legal value : int 5
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

let rec fromMtoN m n =
  if m>n
    then []
  else m:: fromMtoN (m+1) n;;

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





