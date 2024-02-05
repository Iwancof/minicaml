type exp = 
  (* Literals *)
  | IntLit of int
  | BoolLit of bool

  (* Arithmetic *)
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of exp * exp
  | Neg of exp

  (* Logical *)
  | And of exp * exp
  | Or of exp * exp
  | Not of exp

  (* Comparison *)
  | Eq of exp * exp
  | Neq of exp * exp
  | LessThan of exp * exp
  | GreaterThan of exp * exp
  | LessEq of exp * exp
  | GreaterEq of exp * exp

  (* Control flow *)
  | If of exp * exp * exp

type value = 
  | IntVal of int
  | BoolVal of bool

let rec exp_to_string e = 
  match e with
  | IntLit(i) -> string_of_int i
  | BoolLit(b) -> string_of_bool b
  | Plus(e1, e2) -> "(" ^ (exp_to_string e1) ^ " + " ^ (exp_to_string e2) ^ ")"
  | Minus(e1, e2) -> "(" ^ (exp_to_string e1) ^ " - " ^ (exp_to_string e2) ^ ")"
  | Times(e1, e2) -> "(" ^ (exp_to_string e1) ^ " * " ^ (exp_to_string e2) ^ ")"
  | Div(e1, e2) -> "(" ^ (exp_to_string e1) ^ " / " ^ (exp_to_string e2) ^ ")"
  | Neg(e1) -> "-" ^ (exp_to_string e1)
  | And(e1, e2) -> "(" ^ (exp_to_string e1) ^ " && " ^ (exp_to_string e2) ^ ")"
  | Or(e1, e2) -> "(" ^ (exp_to_string e1) ^ " || " ^ (exp_to_string e2) ^ ")"
  | Not(e1) -> "!" ^ (exp_to_string e1)
  | Eq(e1, e2) -> "(" ^ (exp_to_string e1) ^ " == " ^ (exp_to_string e2) ^ ")"
  | Neq(e1, e2) -> "(" ^ (exp_to_string e1) ^ " != " ^ (exp_to_string e2) ^ ")"
  | LessThan(e1, e2) -> "(" ^ (exp_to_string e1) ^ " < " ^ (exp_to_string e2) ^ ")"
  | GreaterThan(e1, e2) -> "(" ^ (exp_to_string e1) ^ " > " ^ (exp_to_string e2) ^ ")"
  | LessEq(e1, e2) -> "(" ^ (exp_to_string e1) ^ " <= " ^ (exp_to_string e2) ^ ")"
  | GreaterEq(e1, e2) -> "(" ^ (exp_to_string e1) ^ " >= " ^ (exp_to_string e2) ^ ")"
  | If(e1, e2, e3) -> "if " ^ (exp_to_string e1) ^ " then " ^ (exp_to_string e2) ^ " else " ^ (exp_to_string e3)

let rec eval e = 
  let expect_int v = 
    match v with
    | IntVal(i) -> i
    | _ -> failwith "Type error(expect_int)" in
  let expect_bool v = 
    match v with
    | BoolVal(b) -> b
    | _ -> failwith "Type error(expect_bool)" in
  let rec int_binop f e1 e2 = IntVal(f (expect_int (eval e1)) (expect_int (eval e2))) in
  let rec int_comp f e1 e2 = BoolVal(f (expect_int (eval e1)) (expect_int (eval e2))) in
  let rec bool_binop f e1 e2 = BoolVal(f (expect_bool (eval e1)) (expect_bool (eval e2))) in

  match e with
  | IntLit(i) -> IntVal(i)
  | BoolLit(b) -> BoolVal(b)
  | Plus(e1, e2) -> int_binop ( + ) e1 e2
  | Minus(e1, e2) -> int_binop ( - ) e1 e2
  | Times(e1, e2) -> int_binop ( * ) e1 e2
  | Div(e1, e2) -> int_binop ( / ) e1 e2
  | Neg(e1) -> IntVal(- (expect_int (eval e1)))
  | And(e1, e2) -> bool_binop ( && ) e1 e2
  | Or(e1, e2) -> bool_binop ( || ) e1 e2
  | Not(e1) -> BoolVal(not (expect_bool (eval e1)))
  | Eq(e1, e2) -> 
    (match (eval e1, eval e2) with
    | (IntVal(i1), IntVal(i2)) -> BoolVal(i1 = i2)
    | (BoolVal(b1), BoolVal(b2)) -> BoolVal(b1 = b2)
    | _ -> failwith "Type error(Eq)")
  | Neq(e1, e2) ->
    (match (eval e1, eval e2) with
    | (IntVal(i1), IntVal(i2)) -> BoolVal(i1 <> i2)
    | (BoolVal(b1), BoolVal(b2)) -> BoolVal(b1 <> b2)
    | _ -> failwith "Type error(Neq)")
  | LessThan(e1, e2) -> int_comp ( < ) e1 e2
  | GreaterThan(e1, e2) -> int_comp ( > ) e1 e2
  | LessEq(e1, e2) -> int_comp ( <= ) e1 e2
  | GreaterEq(e1, e2) -> int_comp ( >= ) e1 e2
  | If(e1, e2, e3) ->
      match (eval e1) with
      | BoolVal(true) -> eval e2
      | BoolVal(false) -> eval e3
      | _ -> failwith "Type error(If)"

let add_plus_2 e = Plus(e, IntLit(2));;

let test_bool b =
  if b then "OK\n" else "FAIL\n";;

print_string (test_bool ((eval (add_plus_2 (IntLit(3)))) = IntVal(5)));;
print_string (test_bool ((eval (add_plus_2 (IntLit(0)))) = IntVal(2)));;
print_string (test_bool ((eval (Div(IntLit(4), IntLit(2))) = IntVal(2))));;

print_string (test_bool ((eval (If(BoolLit(true), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval (If(BoolLit(false), IntLit(1), IntLit(2))) = IntVal(2))));;
print_string (test_bool ((eval (If(Eq(IntLit(1), IntLit(1)), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval (If(Eq(IntLit(1), IntLit(2)), IntLit(1), IntLit(2))) = IntVal(2))));;
print_string (test_bool ((eval (If(LessThan(IntLit(1), IntLit(2)), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval (If(LessThan(IntLit(2), IntLit(1)), IntLit(1), IntLit(2))) = IntVal(2))));;
print_string (test_bool ((eval (If(GreaterThan(IntLit(2), IntLit(1)), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval (If(GreaterThan(IntLit(1), IntLit(2)), IntLit(1), IntLit(2))) = IntVal(2))));;
print_string (test_bool ((eval (If(LessEq(IntLit(1), IntLit(1)), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval (If(LessEq(IntLit(2), IntLit(1)), IntLit(1), IntLit(2))) = IntVal(2))));;
print_string (test_bool ((eval (If(And(BoolLit(true), BoolLit(true)), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval (If(And(BoolLit(true), BoolLit(false)), IntLit(1), IntLit(2))) = IntVal(2))));;
print_string (test_bool ((eval (If(Or(BoolLit(true), BoolLit(false)), IntLit(1), IntLit(2))) = IntVal(1))));;
print_string (test_bool ((eval (If(Or(BoolLit(false), BoolLit(false)), IntLit(1), IntLit(2))) = IntVal(2))));;

let _ = eval (Eq (BoolLit true, IntLit 1));;
