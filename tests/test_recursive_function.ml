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

(*
let rec f x = x in 0
let rec f x = x in f 0
let rec f x = if x = 0 then 1 else 2 + f (x + (- 1)) in f 0
let rec f x = if x = 0 then 1 else x * f (x + (- 1)) in f 3
let rec f x = if x = 0 then 1 else x * f (x + (- 1)) in f 5
*)

print_string (test_bool (run "let rec f x = x in 0" = IntVal 0));;
print_string (test_bool (run "let rec f x = x in f 0" = IntVal 0));;
print_string (test_bool (run "let rec f x = if x = 0 then 1 else 2 + f (x + (- 1)) in f 0" = IntVal 1));;
print_string (test_bool (run "let rec f x = if x = 0 then 1 else (let y = f 0 in x + y) in f 5" = IntVal 6));;
print_string (test_bool (run "let rec f x = if x = 0 then 1 else x * f (x + (- 1)) in f 3" = IntVal 6));;
print_string (test_bool (run "let rec f x = if x = 0 then 1 else x * f (x + (- 1)) in f 5" = IntVal 120));;

(* overwrite args *)

exit(0);;

