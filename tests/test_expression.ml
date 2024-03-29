open Eval;;
open Syntax;;

let add_plus_2 e = Plus(e, IntLit(2));;

let test_bool b =
  if b then "OK\n" else exit(1);;

let eval_with_empty_env e = eval e (emptyenv ());;

print_string (test_bool ((eval_with_empty_env (add_plus_2 (IntLit(3)))) = IntVal(5)));;
print_string (test_bool ((eval_with_empty_env (add_plus_2 (IntLit(0)))) = IntVal(2)));;
print_string (test_bool ((eval_with_empty_env (Div(IntLit(4), IntLit(2))) = IntVal(2))));;

print_string (test_bool ((eval_with_empty_env (If(BoolLit(true), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval_with_empty_env (If(BoolLit(false), IntLit(1), IntLit(2))) = IntVal(2))));;
print_string (test_bool ((eval_with_empty_env (If(Eq(IntLit(1), IntLit(1)), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval_with_empty_env (If(Eq(IntLit(1), IntLit(2)), IntLit(1), IntLit(2))) = IntVal(2))));;
print_string (test_bool ((eval_with_empty_env (If(Less(IntLit(1), IntLit(2)), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval_with_empty_env (If(Less(IntLit(2), IntLit(1)), IntLit(1), IntLit(2))) = IntVal(2))));;
print_string (test_bool ((eval_with_empty_env (If(Greater(IntLit(2), IntLit(1)), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval_with_empty_env (If(Greater(IntLit(1), IntLit(2)), IntLit(1), IntLit(2))) = IntVal(2))));;
print_string (test_bool ((eval_with_empty_env (If(LessEq(IntLit(1), IntLit(1)), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval_with_empty_env (If(LessEq(IntLit(2), IntLit(1)), IntLit(1), IntLit(2))) = IntVal(2))));;
print_string (test_bool ((eval_with_empty_env (If(And(BoolLit(true), BoolLit(true)), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval_with_empty_env (If(And(BoolLit(true), BoolLit(false)), IntLit(1), IntLit(2))) = IntVal(2))));;
print_string (test_bool ((eval_with_empty_env (If(Or(BoolLit(true), BoolLit(false)), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval_with_empty_env (If(Or(BoolLit(false), BoolLit(false)), IntLit(1), IntLit(2))) = IntVal(2))));;

exit(0);;
