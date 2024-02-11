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

print_string (test_bool (run "[]" = ListVal []));;

print_string (test_bool (run "[1; 2; 3]" = ListVal [IntVal 1; IntVal 2; IntVal 3]));;

print_string (test_bool (run "List.hd [1; 2; 3]" = IntVal 1));;
print_string (test_bool (run "List.tl [1; 2; 3]" = ListVal [IntVal 2; IntVal 3]));;

print_string (test_bool (run "[[1; 2]; [3; 4]]" = ListVal [ListVal [IntVal 1; IntVal 2]; ListVal [IntVal 3; IntVal 4]]));;
print_string (test_bool (run "List.hd [[1; 2]; [3; 4]]" = ListVal [IntVal 1; IntVal 2]));;

print_string (test_bool (run "1 :: [2; 3]" = ListVal [IntVal 1; IntVal 2; IntVal 3]));;
print_string (test_bool (run "1 :: 2 :: 3 :: []" = ListVal [IntVal 1; IntVal 2; IntVal 3]));;

print_string (test_bool (run "
let get_two_elms = 
  fun l -> 
    let x = List.hd l in 
    let y = List.hd (List.tl l) 
    in x :: y :: [] 
  in
  get_two_elms [1; 2; 3; 4]" = ListVal [IntVal 1; IntVal 2]));;

exit(0);;
