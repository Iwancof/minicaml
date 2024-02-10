open Eval;;
open Syntax;;

let parse str = 
  Myparser.main Mylexer.token 
    (Lexing.from_string str)

let run str = 
  let e = parse str in
  Eval.eval e (Eval.emptyenv ());;

let test_bool b =
  if b then "OK\n" else exit(1);;

(* simple function apply *)
print_string (test_bool (run "
let f = fun x -> x + 1 in
  f 3
" = IntVal 4));;

print_string (test_bool (run "
let f = fun x -> if x then 1 else 0 in
  f true
" = IntVal 1));;

(* nested function apply *)
print_string (test_bool (run "
let f = fun x -> 
  let g = fun y -> x + y in
    g 3 in
  f 2
" = IntVal 5));;

(* shadowing *)
print_string (test_bool (run "
let x = 1 in
  let f = fun y -> x + y in
    let x = 2 in
      f (x + 3)
" = IntVal 6));;

(* higher order function *)
print_string (test_bool (run "
let f = fun g -> g 3 in
  let g = fun x -> x + 1 in
    f g
" = IntVal 4));;

(* recursive function *)
(* TODO: fix this *)

exit(0);;

