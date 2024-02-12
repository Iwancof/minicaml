open Eval;;
open Syntax;;
open Main;;

print_string ("demo-1: 1 + 2" ^ "\n");
print_string ((value_to_string (run "1 + 2")) ^ "\n");

print_string ("demo-2: 1 + true" ^ "\n");
print_string ((value_to_string (run "1 + true")) ^ "\n");

print_string ("demo-3: 1 + 2 * true" ^ "\n");
print_string ((value_to_string (run "1 + 2 * true")) ^ "\n");

print_string ("demo-4: if true then 1 else 2" ^ "\n");
print_string ((value_to_string (run "if true then 1 else 2")) ^ "\n");

print_string ("demo-5: if 1 then 1 else 2" ^ "\n");
print_string ((value_to_string (run "if 1 then 1 else 2")) ^ "\n");

print_string ("demo-6: if 1 + 2 then 1 else 2" ^ "\n");
print_string ((value_to_string (run "if 1 + 2 then 1 else 2")) ^ "\n");

print_string ("demo-7: if 1 = 2 || 3 = 3 then 1 else 2" ^ "\n");
print_string ((value_to_string (run "if 1 = 2 || 3 = 3 then 1 else 2")) ^ "\n");

print_string ("demo-8: if 1 = 2 && 3 = 3 then 1 else 2" ^ "\n");
print_string ((value_to_string (run "if 1 = 2 && 3 = 3 then 1 else 2")) ^ "\n");

print_string ("demo-9: if ~(1 = 2) then 1 else 2" ^ "\n");
print_string ((value_to_string (run "if ~(1 = 2) then 1 else 2")) ^ "\n");

print_string ("demo-10: if 1 <= 1 then 1 else 2" ^ "\n");
print_string ((value_to_string (run "if 1 <= 1 then 1 else 2")) ^ "\n");

print_string ("demo-11: let x = 1 in x + 2" ^ "\n");
print_string ((value_to_string (run "let x = 1 in x + 2")) ^ "\n");

print_string ("demo-12: let x = 1 in let y = 2 in x + y" ^ "\n");
print_string ((value_to_string (run "let x = 1 in let y = 2 in x + y")) ^ "\n");

print_string ("demo-13: x" ^ "\n");
print_string ((value_to_string (run "x")) ^ "\n");

print_string ("demo-14: let add_one = fun x -> x + 1 in add_one 2" ^ "\n");
print_string ((value_to_string (run "let add_one = fun x -> x + 1 in add_one 2")) ^ "\n");

print_string ("demo-15: let v = 10 in let add_v = fun x -> x + v in add_v 2" ^ "\n");
print_string ((value_to_string (run "let v = 10 in let add_v = fun x -> x + v in add_v 2")) ^ "\n");

print_string ("demo-16: 1(2)" ^ "\n");
print_string ((value_to_string (run "1(2)")) ^ "\n");

print_string ("demo-17: let rec fact = fun x -> if x = 0 then 1 else x * fact (x - 1) in fact 5" ^ "\n");
print_string ((value_to_string (run "let rec fact x = if x = 0 then 1 else x * fact (x - 1) in fact 5")) ^ "\n");

print_string ("demo-18: []" ^ "\n");
print_string ((value_to_string (run "[]")) ^ "\n");

print_string ("demo-19: 1 :: []" ^ "\n");
print_string ((value_to_string (run "1 :: []")) ^ "\n");

print_string ("demo-20: 1 :: 2 :: []" ^ "\n");
print_string ((value_to_string (run "1 :: 2 :: []")) ^ "\n");

print_string ("demo-21: List.hd [1 ; 2; 3]" ^ "\n");
print_string ((value_to_string (run "List.hd [1; 2; 3]")) ^ "\n");

print_string ("demo-22: List.tl [1 ; 2; 3]" ^ "\n");
print_string ((value_to_string (run "List.tl [1; 2; 3]")) ^ "\n");

print_string ("demo-23: List.hd []" ^ "\n");
print_string ((value_to_string (run "List.hd []")) ^ "\n");

print_string ("demo-24: List.tl []" ^ "\n");
print_string ((value_to_string (run "List.tl []")) ^ "\n");

print_string ("demo-25: List.hd 1" ^ "\n");
print_string ((value_to_string (run "List.hd 1")) ^ "\n");

print_string ("demo-26: List.tl 1" ^ "\n");
print_string ((value_to_string (run "List.tl 1")) ^ "\n");
