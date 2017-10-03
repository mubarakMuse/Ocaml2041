
type 'a btree =
    Empty
  | Node of 'a * 'a btree * 'a btree



let rec maxTree a =
  match a with
  | Empty -> None
  | Node (t,l,r) ->
     if l = Empty then Some t
     else maxTree l
let rec minTree a =
  match a with
  | Empty -> none
  | Node (t,l,r) ->
     if r = Empty then t
     else minTree r

let rec isSearchTree t =
   let bigger i j =
     match j with
     | None -> true
     | (Some j') -> i >= j' in
   let smaller i j =
     match j with
     | None -> true
     | Some j' -> j' >= i in
   match t with
   | Empty -> true
   | Node (i,l,r) ->
         isSearchTree l && isSearchTree r &&
           (bigger i (maxTree l)) && (smaller i (minTree r))
