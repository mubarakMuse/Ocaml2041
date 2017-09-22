let rec gcd a b =
	if a = b then a
 	else
  		if a > b then gcd (a-b) b
  		else gcd a (b-a) ;;

let reduced_form tuple1 =
  match tuple1 with
  |(_,_) ->  0
  |(x,y) -> if gcd x y = 1
    then 0
    else (x,y) ;;

let rec fromMtoN m n =
  if m>n
    then []
  else m:: fromMtoN (m+1) n;;

let rec everyEven list1 =
  match list1 with
  |[] -> []
  |[h::l::t] -> l::everyEven t
  |[h::t]-> t ;;
