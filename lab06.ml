
type empItemTy =
  { name : string ;
    phone : string;
    salary : float
}

let smalldb = [{name = "John"; phone= "x3456", salary= 50.1} ; {name= "Jane"; phone= "x1234"; salary= 107.3} ; {name="Joan"; phone= "unlisted"; salary= 12.7}]

 let rec  find_salary a b =
  match a with
  |[] -> None
  |{name = n; phone = p; salary = s}::t -> if n=b then Some s else find_salary t b


 let rec  find_phno a b =
  match a with
  |[] -> None
  |{name = n; phone = p; salary = s}::t -> if n=b then Some p else find_salary t b
  
