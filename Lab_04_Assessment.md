### Assessment for Lab 04

#### Total score: _9_ / _10_

Run on October 02, 10:22:11 AM.

+  _5_ / _5_ : Pass: Check that file "lab04.ml" exists.

+  _2_ / _2_ : Pass: Check that an OCaml file "lab04.ml" has no syntax or type errors.

    OCaml file "lab04.ml" has no syntax or type errors.



+  _0_ / _1_ : Fail: 
Check that the result of evaluating
   ```
   sumList [1;2;3]
   ```
   matches the pattern `6`.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24m[1;31mError[0m: Unbound value sumList
Hint: Did you mean sumlist?
`


+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   perimeter (Circ ((1.0, 1.0), 1.0))
   ```
   matches the pattern `2.0 *. pi`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   perimeter (Triangle ((0.0, 0.0), (3.0, 0.0), (0.0, 4.0)))
   ```
   matches the pattern `12.0`.

   




#### Total score: _9_ / _10_

