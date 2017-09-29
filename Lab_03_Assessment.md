### Assessment for Lab 03

#### Total score: _5_ / _10_

Run on September 28, 21:03:47 PM.

+  _5_ / _5_ : Pass: Check that file "lab03.ml" exists.

+  _0_ / _1_ : Fail: Check that an OCaml file "lab03.ml" has no syntax or type errors.

    OCaml file lab03.ml has errors.

    Run "ocaml lab03.ml" to see them.

    Make sure that you are using ocaml version 4.02.3.  Run "ocaml -version" to check the version number.  Check the specification from Lab 5 again if you are still having problems with this.

+  _0_ / _1_ : Fail: 
Check that the result of evaluating
   ```
   sumup 6
   ```
   matches the pattern `21`.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24m[1;31mError[0m: Unbound value sumup
`


+  _0_ / _1_ : Fail: 
Check that the result of evaluating
   ```
   flip_pair (1, 3)
   ```
   matches the pattern `(3, 1)`.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24m[1;31mError[0m: Unbound value flip_pair
`


+  _0_ / _1_ : Fail: 
Check that the result of evaluating
   ```
   flip_list [(1, 3); (2, 4)]
   ```
   matches the pattern `[(3, 1); (4, 2)]`.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24m[1;31mError[0m: Unbound value flip_list
`


+  _0_ / _1_ : Fail: 
Check that the result of evaluating
   ```
   unzip [(1,2); (3,4); (5,6)]
   ```
   matches the pattern `([1; 3; 5], [2; 4; 6])`.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24m[1;31mError[0m: Unbound value unzip
`


#### Total score: _5_ / _10_

