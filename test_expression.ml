open Expression;;

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