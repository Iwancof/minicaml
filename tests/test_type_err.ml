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

print_string (test_bool (run "1 + true" = TypeErr( BinOpTypeErr(OPlus, IntVal(1), BoolVal(true)))));
print_string (test_bool (run "(1 + 1) + true" = TypeErr( BinOpTypeErr(OPlus, IntVal(2), BoolVal(true)))) );

(* print_string (test_bool (run "1 && true" = BinOpTypeErr("&&", IntVal(1), BoolVal(true)))); *)

print_string (test_bool (run "1 = true" = TypeErr( BinOpTypeErr(OEq, IntVal(1), BoolVal(true)))) );

print_string (test_bool (run "1 < true" = TypeErr( BinOpTypeErr(OGreater, IntVal(1), BoolVal(true)))) );
print_string (test_bool (run "1 > true" = TypeErr( BinOpTypeErr(OLess, IntVal(1), BoolVal(true)))) );

(*
print_string (test_bool (run "1 <= true" = BinOpTypeErr("<=", IntVal(1), BoolVal(true))));
print_string (test_bool (run "1 >= true" = BinOpTypeErr(">=", IntVal(1), BoolVal(true))));
*)

print_string (test_bool (run "if 1 then 2 else 3" = TypeErr( IfTypeErr(IntVal(1)))) );

print_string (test_bool (run "1(2)" = TypeErr( NotAFunctionErr(IntVal(1), IntVal(2)))) );

(* Division by zero *)
print_string (test_bool (run "1 / 0" = DivByZeroErr(IntVal(1))));

exit(0);;
