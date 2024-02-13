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

print_string (test_bool (get_type "1 + 2" = IntTy));
print_string (test_bool (get_type "1 + true" = BinOpTypeErr(OPlus, IntTy, BoolTy)));
print_string (test_bool (type_ok "1 + 2 * 3"));
print_string (test_bool (type_err "1 + 2 * true"));

print_string (test_bool (get_type "1 = 1" = BoolTy));
print_string (test_bool (get_type "1 = true" = BinOpTypeErr(OEq, IntTy, BoolTy)));
print_string (test_bool (get_type "true = false" = BoolTy));
print_string (test_bool (get_type "1 = 2 = 3" = BinOpTypeErr(OEq, BoolTy, IntTy)));
print_string (test_bool (get_type "[] = []" = BoolTy));
print_string (test_bool (get_type "1 = []" = BinOpTypeErr(OEq, IntTy, EmptyListTy)));
print_string (test_bool (get_type "1 = [1]" = BinOpTypeErr(OEq, IntTy, ListTy(IntTy))));
print_string (test_bool (get_type "[1] = [false]" = BinOpTypeErr(OEq, IntTy, BoolTy)));
print_string (test_bool (get_type "[1] = [1]" = BoolTy));
print_string (test_bool (get_type "[1] = [1; 2]" = BoolTy));
print_string (test_bool (get_type "[] = [1]" = BinOpTypeErr(OEq, EmptyListTy, ListTy(IntTy))));

print_string (test_bool (get_type "1 <> 1" = BoolTy));
print_string (test_bool (get_type "1 <> true" = BinOpTypeErr(ONeq, IntTy, BoolTy)));
print_string (test_bool (get_type "true <> false" = BoolTy));
print_string (test_bool (get_type "1 <> 2 <> 3" = BinOpTypeErr(ONeq, BoolTy, IntTy)));
print_string (test_bool (get_type "[] <> []" = BoolTy));
print_string (test_bool (get_type "1 <> []" = BinOpTypeErr(ONeq, IntTy, EmptyListTy)));
print_string (test_bool (get_type "1 <> [1]" = BinOpTypeErr(ONeq, IntTy, ListTy(IntTy))));
print_string (test_bool (get_type "[1] <> [false]" = BinOpTypeErr(ONeq, IntTy, BoolTy)));
print_string (test_bool (get_type "[1] <> [1]" = BoolTy));
print_string (test_bool (get_type "[1] <> [1; 2]" = BoolTy));
print_string (test_bool (get_type "[] <> [1]" = BinOpTypeErr(ONeq, EmptyListTy, ListTy(IntTy))));

print_string (test_bool (type_ok "1 < 2"));
print_string (test_bool (type_err "1 < true"));
print_string (test_bool (type_ok "1 <= 2"));
print_string (test_bool (type_err "1 <= true"));
print_string (test_bool (type_ok "1 > 2"));
print_string (test_bool (type_err "1 > true"));
print_string (test_bool (type_ok "1 >= 2"));
print_string (test_bool (type_err "1 >= true"));

print_string (test_bool (get_type "if true then 1 else 2" = IntTy));
print_string (test_bool (get_type "if 1 then 1 else 2" = IfCondTypeErr(IntTy)));
print_string (test_bool (get_type "if true then 1 else true" = IfArmTypeErr(IntTy, BoolTy)));
print_string (test_bool (type_ok "if true || false then 1 + 1 else 2"));
print_string (test_bool (type_ok "if true then if false then 1 else 2 else 3"));

print_string (test_bool (get_type "let x = 1 in x + 2" = IntTy));
print_string (test_bool (get_type "let x = 1 in x + true" = BinOpTypeErr(OPlus, IntTy, BoolTy)));
print_string (test_bool (type_ok "let x = 1 in let y = x + 2 in y * 3"));

exit(0);;
