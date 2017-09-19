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
      |  (x::l) ->
	 destutter (l)
								    
 


