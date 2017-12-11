                 (* Copyright (c) Gopalan Nadathur *)

(* Homework 6, CSci 2041, Fall 2017
   This file provides code to be used in building a function for reading
   floating point numbers as described in the homework write-up.
   The task is to fill in the definitions of the functions getWhole and
   getFrac and to use these in defining the function getFloat, the real
   objective in this problem *)

(* a reference identifier that provides a lookahead on an input channel *)
let ch = ref ' '

(* moving the next character from an input channel into the lookahead
   getChar : in_channel -> unit *)
let getChar infile = (ch := input_char infile)

(* looking up the next character on the input channel
   lookupChar : unit -> char *)
let lookupChar () = !ch

(* A useful function for skipping over blank spaces in the input channel
   skipSpace : in_channel -> unit *)
let rec skipSpace infile =
  if (lookupChar () = ' ') then (getChar infile; skipSpace infile)
  else ()

(* checking if a given character is a digit
    isDigit : char -> bool *)
let isDigit ch = (ch >= '0' && ch <= '9')

(* Solution to Problem 1
    getWhole : in_channel -> int *)
(* A function for reading a sequence of digits from an input channel
   and interpreting them as a whole number
   Precondition: the "cursor" on the input channel is over the first digit
   getWhole : in_channel -> int *)
let getWhole infile =
  let new1 = ref ' ' in
  let rec getnum num =
    (new1 := lookupChar () ; (* store the first digit*)
      getChar infile;
      if ((lookupChar () = ' ') || (lookupChar () = '.') || not(isDigit(lookupChar ()))) (* check if the next thing isnt a digit *)
      then (num * 10 +(int_of_char !new1 - int_of_char '0'))(* finalize my  answer and return it *)
       else getnum (num * 10 +(int_of_char !new1 - int_of_char '0'))) (* else store the current value and move on to thenext digit*)
  in getnum (int_of_char '0'-int_of_char '0')(* start at zero *)

   (* Just a stub for now, filling this in is part of the homework.
      The work done in Lab 11 should be useful in this. *)

(* SCORE (Problem 2.1 -- getWhole):   4/4
*     2/2 : read entire number up to decimal
*     2/2 : stopped cursor at decimal
*)

(* Solution to Problem 2
    getFrac : in_channel -> float *)
(* A function for reading a sequence of digits from an input channel
   and interpreting them as the fractional part of a floating point number
   Precondition: the "cursor" on the input channel is over the first digit
   getFrac : in_channel -> float *)
   (* this entire is identical to my getwhole except that insted of multplying by an increasing number of multple 10*)
 let rec getFrac infile =
  let new1 = ref ' ' in
  let rec getnum num multi =
    (new1 := lookupChar () ;
      getChar infile;
      if ((lookupChar () = ' ') || (lookupChar () = '.') || not(isDigit(lookupChar ())))
      then (num +. (float_of_int((int_of_char !new1 - int_of_char '0'))/.multi))
       else getnum (num +. (float_of_int((int_of_char !new1 - int_of_char '0'))/.multi)) (multi*.10.0)) (* making space for the next number*)
  in getnum (float_of_int(int_of_char '0'-int_of_char '0')) 10.0

  (* Just a stub for now, filling this in is part of the homework.
     The key idea: as you move to the right, the number has to be
       divided once more by 10. *)


(* Problem 2.2 Score:
  2/2:
  2/2:
*)

(* Solution to Problem 3
    getFloat : in_channel -> float option *)
(* A function for reading in a floating point number, in the format
   described in the homework writeup, and returning it using the
   option type. The function should return None if the next
   token in the input is not a floating point number
    getFloat : in_channel -> float option *)
(* 2.3:
  2/2
  2/2
*)

let getFloat infile =
  (skipSpace infile; (* to get to the first digit or deceimal point*)
    if (isDigit(lookupChar ())) (* found a number first*)
      then (let x = float_of_int( getWhole infile) (* get the float value of whats before the decimal or the end*)
          in (if (lookupChar () = '.') (* decimal found*)
                then (getChar infile; (* move pass the decimal *)
                    if (isDigit(lookupChar ())) (** found a number first **)
                    then ( Some (x +. getFrac infile)) (* calucatue the fractonal value of what after the decimal and add it to thhe whole we had*)
                    else Some x) (* nothing behind the decinal so just return x*)
                else None)) (* bad input becasue theres no decimal point *)
    else if (lookupChar () = '.') (* if there nothing before the decicaml*)
            then (getChar infile;
                    if (isDigit(lookupChar ()))
                      then ( Some (getFrac infile))
                      else None)
      else  None)



  (* Here is roughly the logic that needs to be implemented
      1. skip over the initial blank characters
      2. if the first non-blank is a digit, get the
         whole number part using the function getWhole
      3. check that the next character is a decimal point
      4. if the next character is a digit, get the fractional
         part using getFrac
      5. Combine the whole number and the fractional part to
         get the floating point number n and return (Some n)
      6. If the decimal point or both the whole number and
         fractional parts are missing, then return None *)


(* A function to test getWhole *)
let test_getWhole () =
  (Printf.printf "Enter a number: "; flush stdout;
   ch := ' ';
   skipSpace stdin;
   if (isDigit (lookupChar ()))
   then Printf.printf "Your input: %d\n" (getWhole stdin)
   else Printf.printf "Bad input\n")

(* A function to test getFrac *)
let test_getFrac () =
  (Printf.printf "Enter a number: "; flush stdout;
   ch := ' ';
   skipSpace stdin;
   if (isDigit (lookupChar ()))
   then Printf.printf "Your input: %f\n" (getFrac stdin)
   else Printf.printf "Bad input\n")

(* a function to test getFloat *)
let test_getFloat () =
  (Printf.printf "Enter a number: "; flush stdout;
   ch := ' ';
   match (getFloat stdin) with
   | None -> Printf.printf "Bad input\n"
   | (Some r) -> Printf.printf "Your input: %f\n" r)
