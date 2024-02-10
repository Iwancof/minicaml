open Syntax ;;
open Eval ;;

(* 与えられた文字列の字句解析と構文解析だけを行う関数 *)
(* parse : string -> exp *)

let parse str = 
  Myparser.main Mylexer.token 
    (Lexing.from_string str)

let run str = 
  let e = parse str in
  Eval.eval e (Eval.emptyenv ());;

let test_bool b =
  if b then "OK\n" else exit(1);;

print_string (test_bool (run "1" = IntVal 1));;
print_string (test_bool (run ("1 + 2 * 3") = IntVal 7));;
print_string (test_bool (run ("(1 + 2) * 3") = IntVal 9));;
print_string (test_bool (run ("(1 + 2) * (3 + 4)") = IntVal 21));;
print_string (test_bool (run ("if 1 < 2 then 3 else 4") = IntVal 3));;
print_string (test_bool (run ("if 1 > 2 then 3 else 4") = IntVal 4));;
print_string (test_bool (run ("let x = 1 in x + 2") = IntVal 3));;
print_string (test_bool (run ("let x = 1 in let y = 2 in x + y") = IntVal 3));;
print_string (test_bool (run ("let x = 1 in let y = 2 in x + y * x") = IntVal 3));;

exit(0);;
