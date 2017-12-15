 type 'a mylist = 'a listcell ref
and  'a listcell = Nil | Cons of 'a * ('a mylist) ref

let rec reverse l acc = 
	match  !l with
	| Nil ->  acc
	| Cons (a,b) -> let nextL = !b in (b := acc  ; reverse (nextL) (l))	

let readnum infile =
  let rec skip_space () =
    let ch = input_char infile
    in if (ch = ' ') then skip_space ()
       else ch in
  let is_digit ch = (ch >= '0' && ch <= '9') in
  let rec getnum num =
  	let ch = input_char infile in 
  	if is_digit(ch) then let new1 = (num*10) + (int_of_char ch - int_of_char '0') in 
  	(getnum new1)  
  	else num    
   in let ch = skip_space ()
      in if (is_digit ch)
         then Some (getnum (int_of_char ch - int_of_char '0'))
         else None 
let get_num_from_user () =
  (Printf.printf "Enter a number: "; flush stdout;
   match (readnum stdin) with
   | None -> Printf.printf "Bad input\n"
   | (Some n) -> Printf.printf "Your input: %d\n" n)