
type 'a btree =
    Empty
  | Node of 'a * 'a btree * 'a btree



let rec maxTree a =
  match a with
  | Empty -> None
  | Node (t,l,r) ->
     if r = Empty then Some t
     else maxTree r
let rec minTree a =
  match a with
  | Empty -> None
  | Node (t,l,r) ->
     if l = Empty then Some t
     else minTree l

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

let t1 = Node(10, Node(9,Empty,Empty),Node(12,Empty,Empty))
let t2 = Node(10, Node(12,Empty,Empty),Node(9,Empty,Empty))
let t3 = Node(10, Node(2,Empty,Empty),Node(100,Empty,Empty))


type  expr' =
  Int' of int | True' | False'
| Plus' of expr' * expr' | Minus' of expr' * expr'
| Lss' of expr' * expr' | Gtr' of expr' * expr'
| And' of expr' * expr' | Or' of expr' * expr'
| Cond' of expr' * expr' * expr'

type ty = IntTy | BoolTy 

let rec typeof a = 
  match a with
  | Int' a -> Some IntTy 
  | True' -> Some BoolTy
  | False' -> Some BoolTy
  | Plus'(e1,e2) -> if (typeof e1 = Some IntTy && typeof e2 = Some IntTy ) then Some IntTy else None
  | Minus'(e1,e2)-> if (typeof e1 = Some IntTy && typeof e2 = Some IntTy) then Some IntTy else None
  | Lss'(e1, e2) -> if (typeof e1 = Some IntTy && typeof e2 = Some IntTy) then Some BoolTy else None
  | Gtr'(e1, e2) -> if (typeof e1 = Some IntTy && typeof e2 = Some IntTy ) then Some BoolTy else None
  | And'(e1, e2) -> if (typeof e1 = Some BoolTy && typeof e2 = Some BoolTy)  then Some BoolTy else None
  | Or'(e1, e2) -> if (typeof e1 = Some BoolTy && typeof e2 = Some BoolTy ) then Some BoolTy else None
  | Cond'(e1,e2,e3) -> if (typeof e1 = Some BoolTy && typeof e2 = Some IntTy && typeof e2 = Some IntTy ) then Some BoolTy else None


let wellTyped a = 
  match ( typeof a) with
  | None -> false
  | _ -> true

  let e1 = Cond' (Lss' (Int' 10, Plus' (Int' 5, Int' 7)),
                  Int' 5, Int' 7)

  let e2 = Cond' (Lss' (Int' 10, Plus' (Int' 5, Int' 7)),
                  Int' 5, True')

  let e3 = Cond' (And' (Lss' (Int' 10, Plus' (Int' 5, Int' 7)), True'),
                  False', True')


  type ocamlTy = 
  | IntTy' of int 
  | BoolTy' of bool 
  | NameTy of string 
  | ListTy of ocamlTy 
  | FunTy of ocamlTy * ocamlTy






