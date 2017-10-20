
type empItemTy =
  { name : string ;
    phone : string;
    salary : float
}

let smalldb = [{name = "John"; phone = "x3456"; salary= 50.1} ; {name= "Jane"; phone= "x1234"; salary= 107.3} ; {name="Joan"; phone= "unlisted"; salary= 12.7}]

 let rec  find_salary a b =
  match a with
  |[] -> None
  |{name = n; phone = p; salary = s}::t -> if n=b then Some s else find_salary t b


 let rec  find_phno a b =
  match a with
  |[] -> None
  |{name = n; phone = p; salary = s}::t -> if n=b then Some p else find_phno t b
  
type 'a nestedItem = Single of 'a 
| NestedItem of 'a nestedItem list

type 'a nestedList = 'a nestedItem list


let intlist1 = NestedItem[ Single 1; NestedItem [Single 2; NestedItem[Single 3]; Single 4]; NestedItem[ NestedItem[Single 5; Single 6; NestedItem [Single 7]]]]



let rec flatten exp = 
 	match exp with
 	| NestedItem []-> []
 	| Single a ->  [a]
 	| NestedItem (h::t) -> (flatten h ) @ (flatten (NestedItem t))


type form = 
| Pos of string 
| Neg of string
| And of form * form
| Or of form * form

let rec cnf exp = 
	match exp with 
	| Pos e1 -> Pos e1
	| Neg e1 -> Neg e1
	| And (e1, e2) -> Or (e1 ,e2)
	| Or(And (e1, e2),e3) -> And ( cnf (And(e1,e3)), cnf (And(e2,e3)))
	| Or(e1,And(e2,e3))-> And (cnf (And(e1,e2)),cnf (And (e1,e3)))
	| Or(e1, e2) -> Or(e1, e2)




let f1 = Or ( And ( Neg "F", Pos "G"), Pos "H")
let f2 = Or ( Pos "H",And ( Neg "F", Pos "G"))
(*
-> And ( Or ( Neg "F", Pos "H") , Or (Pos "G", Pos "H") )



*)