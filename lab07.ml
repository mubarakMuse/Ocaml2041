let rec map2 f l u =
  match(l,u) with
  |([],[]) -> []
  |(h::t,[]) -> []
  |([],h::t) -> []
  |(h1::t1,h2::t2) -> f h1 h2:: map2 f t1 t2

let rec accumulate f lst u =
  match lst with
  | [] -> u
  | (h::t) -> accumulate f t (f h u)

let rec reduce f lst u =
  match lst with
  | [] -> u
  | (h::t) -> f h (reduce f t u)
    
