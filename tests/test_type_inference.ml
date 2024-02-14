open Eval;;
open Type;;
open Syntax;;

let parse str = 
  Myparser.main Mylexer.token 
    (Lexing.from_string str)

let run str = 
  let e = parse str in
  Eval.eval e (Eval.emptyenv ());;

let get_type str = 
  let e = parse str in
  Type.typeof e (Eval.emptyenv ());;

let type_ok str =
  match get_type str with
  | IntTy -> true
  | BoolTy -> true
  | ListTy(_) -> true
  | EmptyListTy -> true
  | _ -> false

let type_err str =
  match get_type str with
  | IntTy -> false
  | BoolTy -> false
  | ListTy(_) -> false
  | EmptyListTy -> false
  | _ -> true

let test_bool b =
  if b then "OK\n" else exit(1);;

print_string (test_bool (get_type "1 + 2" = IntTy));;
print_string (test_bool (get_type "fun x -> if x then 1 else 2" = ArrowTy(BoolTy, IntTy)));;

print_string (test_bool (get_type "fun t -> fun f -> fun x -> if x then t else f + 1" = ArrowTy(IntTy, ArrowTy(IntTy, ArrowTy(BoolTy, IntTy)))));;

exit(0);;
