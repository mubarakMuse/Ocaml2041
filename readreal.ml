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
    (new1 := lookupChar () ; 
      getChar infile; 
      if ((lookupChar () = ' ') || (lookupChar () = '.') || not(isDigit(lookupChar ()))) 
      then (num * 10 +(int_of_char !new1 - int_of_char '0'))
       else getnum (num * 10 +(int_of_char !new1 - int_of_char '0'))) 
  in getnum (int_of_char '0'-int_of_char '0')
 
   (* Just a stub for now, filling this in is part of the homework.
      The work done in Lab 11 should be useful in this. *)


(* Solution to Problem 2
    getFrac : in_channel -> float *)
(* A function for reading a sequence of digits from an input channel
   and interpreting them as the fractional part of a floating point number
   Precondition: the "cursor" on the input channel is over the first digit
   getFrac : in_channel -> float *)
 let rec getFrac infile = 
  let new1 = ref ' ' in 
  let rec getnum num multi =
    (new1 := lookupChar () ; 
      getChar infile; 
      if ((lookupChar () = ' ') || (lookupChar () = '.') || not(isDigit(lookupChar ()))) 
      then (num +. (float_of_int((int_of_char !new1 - int_of_char '0'))/.multi))
       else getnum (num +. (float_of_int((int_of_char !new1 - int_of_char '0'))/.multi)) (multi*.10.0))
  in getnum (float_of_int(int_of_char '0'-int_of_char '0')) 10.0
 
  (* Just a stub for now, filling this in is part of the homework.
     The key idea: as you move to the right, the number has to be
       divided once more by 10. *)
   


(* Solution to Problem 3
    getFloat : in_channel -> float option *)
(* A function for reading in a floating point number, in the format
   described in the homework writeup, and returning it using the
   option type. The function should return None if the next
   token in the input is not a floating point number
    getFloat : in_channel -> float option *)
let getFloat infile =
  (skipSpace infile; 
    if (isDigit(lookupChar ())) 
      then (let x = float_of_int( getWhole infile)
          in (if (lookupChar () = '.') 
                then (getChar infile; 
                    if (isDigit(lookupChar ())) 
                    then ( Some (x +. getFrac infile)) 
                    else Some x)
                else None))
    else if (lookupChar () = '.') 
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


