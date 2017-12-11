                 (* Copyright (c) Gopalan Nadathur *)

(* Some functions for displaying colourings *)

(* Printing a list using a function for printing items in the list *)
let printlist item_printer l =
   let rec printlist_aux l =
     match l with
     | [] -> Printf.printf "%c" ']'
     | (h::t) -> Printf.printf "%s" ", ";
                 item_printer h;
                 printlist_aux t
   in (Printf.printf "%c" '[';
       match l with
       | (h::t) -> item_printer h; printlist_aux t
       | _ -> printlist_aux l)

(* A function for displaying an integer item *)
let int_printer i = Printf.printf "%d" i

(* A function for displaying a colour (represented by a string) *)
let show_color c = Printf.printf "%s" c

(* A function for displaying a node, colour pair *)
let show_node_and_color (n,c) =
   Printf.printf "(%d," n; show_color c; Printf.printf ")"

(* A function for showing a (complete) colouring *)
let show_coloring l =
   Printf.printf "\nColoring for the graph: "; printlist show_node_and_color l;
   Printf.printf "\n"


(* Solution to Problem 1 *)
(* Problem 1 Specific Code *)

exception Search_Failure

let ask_user printer config =
   printer config;
   Printf.printf "More solutions (y/n)? ";
   if (read_line () = "y")
   then (raise Search_Failure)
   else ()
 (* method that returns the color of a node already in the colored list  *)
let rec check_colored node colored color =
  match colored with
  | [] -> " "
  | (n,c)::t -> if n== node then c
                else check_colored node t color
(* method that checks if the nieghbor of the node in question
has been already colored with the color we want to color it with *)
 let rec check color node adjlist colored =
   match adjlist with
   | [] -> true
   | h::t -> if check_colored h colored color == color then false
            else true && check color node t colored

(* method returns the niegbors of the node*)
let rec find_adjList node adjacency=
  match adjacency with
  | [] -> []
  | (n,l)::t -> if n == node then l else find_adjList node t

let color_graph nodes adjacency colors =
   let rec color_graph_aux nodes colored =
      let rec helper h t aval_colors = (* function that assigns the right color to a node or raise a failure excpetion*)
        match aval_colors with
        | [] -> raise Search_Failure (* ran out of colors *)
        | c1::c2 -> if (check c1 h (find_adjList h adjacency) colored )
                  then try (color_graph_aux t ((h,c1)::colored)) with
                        | Search_Failure -> helper h t c2 (* backtracks to the first node*)
                    else helper h t c2 (* move on to the next color*)
      in match nodes with
      | [] -> ask_user show_coloring colored (* interacting with user *)

      | h::t -> (helper h t colors) (* first attempt at matching a node with a color*)

    (* this code should try to extend the colouring
       already present in colored into a colouring
       also for all the nodes in nodes. The general
       scheme:
           1. If no more nodes, then colouring has
              succeeded, interact with the user
           2. Else, pick a color for the first node and
              add it to colored
           3. try to color the remaining nodes in the
              extended colored environment
           4. if coloring is unsuccessful, indicated by
              an exception, handle the exception by
              picking another color for the first node
           5. if the colors for the first node have been
              exhausted, raise an exception to signal failure *)
           (* This is just to make the code compile *)
   in try (color_graph_aux nodes []) with
        Search_Failure -> Printf.printf "\nNo (more) colourings possible\n"

(* Problem 3, Part 1 Score:
 * 3.1: 6/6
 +1: using exceptions in fail cases.
 +1: continuing to provide solutions when the user says y.
 +2: producing a correct solution, when one exists.
 +2: for producing all possible solutions.
 *)

(* Solution to Problem 2 *)
(* Problem 2 Code *)

let ask_user_cps printer config succ fail =
   printer config;
   Printf.printf "More solutions (y/n)? ";
   if (read_line () = "y") then (fail ()) else (succ ())

let color_graph_cps nodes adjacency colors =
   let rec color_graph_aux nodes colored succ fail =
   let rec helper_cps h t aval_colors = (* function that assigns the right color to a node or raise a failure excpetion*)
        match aval_colors with
        | [] -> fail()(* ran out of colors *)
        | c1::c2 -> if (check c1 h (find_adjList h adjacency) colored )
                  then (color_graph_aux t ((h,c1)::colored) (fun () -> succ()) (fun () -> helper_cps h t c2)) (* the failure cont. backtracks to the first node and recolors it*)
                    else helper_cps h t c2 (* move on to the next color*)
      in match nodes with
      | [] -> ask_user_cps show_coloring colored succ fail (* interacting with user *)
      | h::t -> (helper_cps h t colors) (* first attempt at matching a node with a color*)


    (* this code should try to extend the colouring
       already present in colored into a colouring
       also for all the nodes in nodes. The general
       scheme:
           1. If no more nodes in nodes, the interact
              with user
           2. If there is a node, pick a color for it
           3. try to color the remaining nodes in the
              extended colored environment, passing
              suitable success and failure continuations;
              the latter should resume by picking another
              color for the current node.
           4. if no color is available for current node
              invoke the failure continuation  *)
           (* This is just to make the code compile *)
   in color_graph_aux nodes [] (fun () -> ())
                               (fun () -> Printf.printf "\nNo (more) colourings\n")

(* Problem 3, Part 2 Score :
 * 3.2: 6/6 *)
