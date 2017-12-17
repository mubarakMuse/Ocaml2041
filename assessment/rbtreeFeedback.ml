 (* Copyright (c) Gopalan Nadathur *)

module type DATA =
   sig
     type item
     val leq : item * item -> bool
      val print : out_channel -> item -> unit
     val eq : item * item -> bool
   end

module type BTREE =
   sig
     type item
     type btree
     val insert : item * btree -> btree
     val print : out_channel -> btree -> unit
     val find : btree*item -> bool
     val initTree : unit -> btree
   end
(* Problem 3 Part 1 : functor that
takes in Data that then satisfies the
properties identified by the DATA signature and produces a structure
satisfying the properties identified by the BTREE signature.*)

module BTree (Data: DATA): (BTREE with type item = Data.item) =
  struct
    type item = Data.item

    type color = R | B

    type btree =
        Empty
      | Node of color * item * btree * btree

    let balance t =
      match t with
      | ( Node(B,z,Node(R,x,Node(R,y,a,b),c),d) |
          Node(B,z,Node(R,y,a,Node(R,x,b,c)),d) |
          Node(B,y,a,Node(R,z,Node(R,x,b,c),d)) |
          Node(B,y,a,Node(R,x,b,Node(R,z,c,d))) ) ->
            Node(R,x,Node(B,y,a,b),Node(B,z,c,d))
      |  _ -> t

    let insert (d,t) =
       let rec ins t =
         match t with
         | Empty -> Node (R, d, Empty, Empty)
         | Node (c,d',l,r) ->
             if (d < d') then balance (Node (c,d',ins l, r))
             else balance (Node (c,d', l,ins r))
       in match (ins t) with
          | Node (_,d,l,r) -> Node (B,d,l,r)
          | Empty -> raise (Invalid_argument "insert")

    let initTree () = Empty

    let rec find =
      function
      | (Empty,i) -> false
      | (Node(_,i',l,r),i) ->
           if Data.eq(i,i') then true
           else if Data.leq(i,i')
                then find (l,i)
                else find (r,i)
    let print outfile bt =
      let rec indent n =
        match n with
        | 0 -> ()
        | n -> (Printf.fprintf outfile "  "; indent (n-1)) in
      let rec print_aux n =
        function
        | Empty -> ()
        | Node (_,i,l,r) ->
           (print_aux (n+1) l;
            indent n; Data.print outfile i; Printf.fprintf outfile "\n";
            print_aux (n+1) r
           )
      in print_aux 0 bt
  end

(* SCORE (Problem 4.1 -- BTree functor) : 5/5
 *      +5 : for correctly defining the BTree functor
 *)

(* Problem 4 part 2 *)
module IntData : (DATA with type item =int)=
struct (* IntData that meets the requirements of DATA.*)
    type item = int
    let leq (x, y) = if y<x then false else true
    let print outfile i =  Printf.fprintf outfile " %d " i
    let eq (x, y) = if y=x then true else false

end

module IntBTree = BTree(IntData) (* Functor  to realize a binary search tree structure over integer data *)

(* Problem 4 part 3 *)

module StringData : (DATA with type item = string)=
struct (* StringData that meets the requirements of DATA.*)
    type item = string
    let leq (x, y) = if y<x then false else true
    let print outfile i =  Printf.fprintf outfile  " %s "  i (* prints without quotation marks around it*)
    let eq (x, y) = if y=x then true else false

end
module StringBTree = BTree(StringData)(* Functor  to realize a binary search tree structure over String data *)

(* Problem 4.2 and 4.3 Score:
 * 4.2: 5/5
 * 4.3: 5/5
 *)
