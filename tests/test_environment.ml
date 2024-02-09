open Eval;;
open Syntax;;

let test_bool b =
  if b then "OK\n" else "FAIL\n";;

let test_int value int =
  test_bool (value = IntVal(int));;

(* using x *)
print_string (test_int (eval (Let("x", IntLit(3), Var("x"))) (emptyenv ())) 3);;

(* using x with operation *)
let e = 
  Let("x", IntLit(3), 
  Let("y", IntLit(4),
  Let("z", IntLit(5),
  Plus(Var("x"), Plus(Var("y"), Var("z"))))));;

print_string (test_int (eval e (emptyenv ())) 12);;

(* overwriting x *)
let e = 
  Let("x", IntLit(3), 
  Let("y", IntLit(4),
  Let("z", IntLit(5),
  Let("x", IntLit(6),
  Var("x")))));;

print_string (test_int (eval e (emptyenv ())) 6);; 


(* overwriting x with x + 1 *)
let e = 
  Let("x", IntLit(3), 
  Let("x", Plus(Var("x"), IntLit(1)),
  Var("x")));;

print_string (test_int (eval e (emptyenv ())) 4);;

print_string "All tests passed.\n";;
