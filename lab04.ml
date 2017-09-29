let rec pairwith x l =
     match l with
     | [] -> []
     | (h::t) -> (x,h) :: pairwith x t
(* it takes an element and a list then pairs each item in nth e list with the element and returns a list of tuples *)
(*
let rec addN l n =
     match l with
     | [] -> []
     | (h::t) -> (h+n) :: addN t
  it missing the int argument in final recursive call *)

(* let rec addN =
     function
      | (_,[]) -> []
      | (n,(h::t)) -> (h+n) :: addN n t
	 
   the recursive call take 2 arguments not one and the way to fix it it by pairing it in a tuple *)

         let rec assoc l x =
     match l with
     | [] -> "Not found"
     | ((y,v)::t) -> if (x = y) then v else assoc t x
(* val assoc : ('a * string) list -> 'a -> string = <fun> *)
   (* let lengthOfHead l =
      let rec length =
             function
               | [] -> 0
               | (h::t) -> 1 + length t
          and lengthofl = length l
      in match l with
         | [] -> 0
         | (h::t) -> length h *)

let atLeastTwo =
    function
      | ([] | [_]) -> false
      | _ -> true
let sameFstAndSnd =
    function
      | ([] | [_]) -> false
      | (h::t::_) -> true
      

let rec sumlist' sum lst1  =
    match lst1 with 
      |[] -> sum
      |h::t -> sumlist' (h+sum) t

let sumlist a =
  let rec sumlist' sum lst1  =
    match lst1 with 
      |[] -> sum
      |h::t -> sumlist' (h+sum) t
  in
    sumlist' 0 a
(* Invarint: sumlist a takes the head of the list and addds it to an on going some then redoes the process with the same list except with out the head that was added to he sum*)

let rec drop (n,l) =
    match l with
    | [] -> []
    | (h::t) -> if (n = 0) then l else drop (n-1, t)

type coord = float * float

type shape =
     | Circ of coord * float
     | Triangle of coord * coord * coord
     | Quadrangle of coord * coord * coord * coord

let pi = 4.0 *. atan 1.0;;

let distance =
    function
      | ((x1,y1),(x2,y2)) ->
            let xdiff = x1 -. x2 in
            let ydiff = y1 -. y2 in
              sqrt (xdiff *. xdiff +. ydiff *. ydiff)


let perimeter = 
  function 
  | Circ ((a,b),r)-> 2.0*.pi*.r
  | Triangle ((a,b),(c,d),(e,f)) -> distance ((a,b),(c,d)) +. distance ((c,d),(e,f)) +. distance ((a,b),(e,f))
  | Quadrangle ((a,b),(c,d),(e,f),(g,h)) -> distance ((a,b),(c,d)) +. 
  distance ((c,d),(e,f)) +. distance ((e,f),(g,h)) +. distance ((g,h),(a,b))


(*	 musex025@csel-kh1250-29:/home/musex025/csci2041/repo-musex025 $ ocaml
        OCaml version 4.02.3

# #use "lab04.ml" ;;
val pairwith : 'a -> 'b list -> ('a * 'b) list = <fun>
val addN : int * int list -> int list = <fun>
# #use "lab04.ml" ;;
val pairwith : 'a -> 'b list -> ('a * 'b) list = <fun>
val assoc : ('a * string) list -> 'a -> string = <fun>
# #use "lab04.ml" ;;
val pairwith : 'a -> 'b list -> ('a * 'b) list = <fun>
val assoc : ('a * string) list -> 'a -> string = <fun>
File "lab04.ml", line 33, characters 28-29:
Error: This expression has type 'a but an expression was expected of type
         'a list
       The type variable 'a occurs inside 'a list
# # let ((ff,fs) as f,s) = ((1,2),3);;
Characters 2-5:
  # let ((ff,fs) as f,s) = ((1,2),3);;
    ^^^
Error: Syntax error
# let ((ff,fs) as f,s) = ((1,2),3);;
val ff : int = 1
val fs : int = 2
val f : int * int = (1, 2)
val s : int = 3
# #use "lab04.ml";;
val pairwith : 'a -> 'b list -> ('a * 'b) list = <fun>
val assoc : ('a * string) list -> 'a -> string = <fun>
val atLeastTwo : 'a list -> bool = <fun>
# #use "lab04.ml" ;;
val pairwith : 'a -> 'b list -> ('a * 'b) list = <fun>
val assoc : ('a * string) list -> 'a -> string = <fun>
val atLeastTwo : 'a list -> bool = <fun>
File "lab04.ml", line 42, characters 12-13:
Error: Variable h is bound several times in this matching
# #use "lab04.ml" ;;
Cannot find file lab04.ml' ;;
.
# #use "lab04.ml" ;;
val pairwith : 'a -> 'b list -> ('a * 'b) list = <fun>
val assoc : ('a * string) list -> 'a -> string = <fun>
val atLeastTwo : 'a list -> bool = <fun>
File "lab04.ml", line 43, characters 8-9:
Warning 11: this match case is unused.
val sameFstAndSnd : 'a list -> bool = <fun>
  #
  *)
