let rec sumup a =
  if (a=0) then 0 else a+sumup(a-1);;

let flip_pair a =
  match a with
  | (x,y) -> (y,x);;

let rec destutter =
    function
      |  [] -> []
      |  (x::y::l) ->
         if (x=y) then destutter (x::l) else x :: destutter (y::l)
      |  (x::l) ->  destutter (l)

let rec sum_diffs list1 =
  fuction
      |[]-> 0
      |[h::l::t] -> (h-l)+sum_diffs [l::t]
      |[h::t] -> h-t + sum_diff []

let unzip list =
  match list1 with
    [] -> []
      [h::t] ->
	match h with
	  (x,y) -> x
