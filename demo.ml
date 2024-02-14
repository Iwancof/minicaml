open Eval;;
open Syntax;;
open Type;;
open Main;;

print_string ("demo: 1 + 2" ^ "\n");
print_string ((pretty_print_value (run "1 + 2")) ^ "\n");

print_string ("demo: 1 + true" ^ "\n");
print_string ((pretty_print_value (run "1 + true")) ^ "\n");

print_string ("demo: 1 + 2 * true" ^ "\n");
print_string ((pretty_print_value (run "1 + 2 * true")) ^ "\n");

print_string ("demo: if true then 1 else 2" ^ "\n");
print_string ((pretty_print_value (run "if true then 1 else 2")) ^ "\n");

print_string ("demo: if 1 then 1 else 2" ^ "\n");
print_string ((pretty_print_value (run "if 1 then 1 else 2")) ^ "\n");

print_string ("demo if 1 + true then 1 else 2" ^ "\n");
print_string ((pretty_print_value (run "if 1 + true then 1 else 2")) ^ "\n");

print_string ("demo: if 1 + 2 then 1 else 2" ^ "\n");
print_string ((pretty_print_value (run "if 1 + 2 then 1 else 2")) ^ "\n");

print_string ("demo: if 1 = 2 || 3 = 3 then 1 else 2" ^ "\n");
print_string ((pretty_print_value (run "if 1 = 2 || 3 = 3 then 1 else 2")) ^ "\n");

print_string ("demo: if 1 = 2 && 3 = 3 then 1 else 2" ^ "\n");
print_string ((pretty_print_value (run "if 1 = 2 && 3 = 3 then 1 else 2")) ^ "\n");

print_string ("demo: if ~(1 = 2) then 1 else 2" ^ "\n");
print_string ((pretty_print_value (run "if ~(1 = 2) then 1 else 2")) ^ "\n");

print_string ("demo: if 1 <= 1 then 1 else 2" ^ "\n");
print_string ((pretty_print_value (run "if 1 <= 1 then 1 else 2")) ^ "\n");

print_string ("demo: let x = 1 in x + 2" ^ "\n");
print_string ((pretty_print_value (run "let x = 1 in x + 2")) ^ "\n");

print_string ("demo: let x = 1 in let y = 2 in x + y" ^ "\n");
print_string ((pretty_print_value (run "let x = 1 in let y = 2 in x + y")) ^ "\n");

print_string ("demo: x" ^ "\n");
print_string ((pretty_print_value (run "x")) ^ "\n");

print_string ("demo: let add_one = fun x -> x + 1 in add_one 2" ^ "\n");
print_string ((pretty_print_value (run "let add_one = fun x -> x + 1 in add_one 2")) ^ "\n");

print_string ("demo: let v = 10 in let add_v = fun x -> x + v in add_v 2" ^ "\n");
print_string ((pretty_print_value (run "let v = 10 in let add_v = fun x -> x + v in add_v 2")) ^ "\n");

print_string ("demo: 1(2)" ^ "\n");
print_string ((pretty_print_value (run "1(2)")) ^ "\n");

print_string ("demo: let rec fact = fun x -> if x = 0 then 1 else x * fact (x - 1) in fact 5" ^ "\n");
print_string ((pretty_print_value (run "let rec fact x = if x = 0 then 1 else x * fact (x - 1) in fact 5")) ^ "\n");

print_string ("demo: []" ^ "\n");
print_string ((pretty_print_value (run "[]")) ^ "\n");

print_string ("demo: 1 :: []" ^ "\n");
print_string ((pretty_print_value (run "1 :: []")) ^ "\n");

print_string ("demo: 1 :: 2 :: []" ^ "\n");
print_string ((pretty_print_value (run "1 :: 2 :: []")) ^ "\n");

print_string ("demo: List.hd [1 ; 2; 3]" ^ "\n");
print_string ((pretty_print_value (run "List.hd [1; 2; 3]")) ^ "\n");

print_string ("demo: List.tl [1 ; 2; 3]" ^ "\n");
print_string ((pretty_print_value (run "List.tl [1; 2; 3]")) ^ "\n");

print_string ("demo: List.hd []" ^ "\n");
print_string ((pretty_print_value (run "List.hd []")) ^ "\n");

print_string ("demo: List.tl []" ^ "\n");
print_string ((pretty_print_value (run "List.tl []")) ^ "\n");

print_string ("demo: List.hd 1" ^ "\n");
print_string ((pretty_print_value (run "List.hd 1")) ^ "\n");

print_string ("demo: List.tl 1" ^ "\n");
print_string ((pretty_print_value (run "List.tl 1")) ^ "\n");

print_string ("demo: 1 * 2 + 2 * (0 :: [1 + 2, 3 + false])" ^ "\n");
print_string ((pretty_print_value (run "1 * 2 + 2 * (0 :: [1 + 2; 3 + false])")) ^ "\n");

print_string ("demo: fun x -> x + 1" ^ "\n");
print_string ((mintype_to_string (get_type ("fun x -> x + 1"))) ^ "\n");

print_string ("demo: fun x -> x && true" ^ "\n");
print_string ((mintype_to_string (get_type ("fun x -> x && true"))) ^ "\n");

print_string ("demo: fun x -> x" ^ "\n");
print_string ((mintype_to_string (get_type ("fun x -> x"))) ^ "\n");

print_string ("demo: let f = fun x -> x in if f 1 then 1 else 2" ^ "\n");
print_string ((mintype_to_string (get_type ("let f = fun x -> x in if f 1 then 1 else 2"))) ^ "\n");

print_string ("demo: let f = fun x -> x in if f true then 1 else 2" ^ "\n");
print_string ((mintype_to_string (get_type ("let f = fun x -> x in if f true then 1 else 2"))) ^ "\n");

print_string ("demo: let f = fun x -> x && true in if f 1 then 1 else 2" ^ "\n");
print_string ((mintype_to_string (get_type ("let f = fun x -> x && true in if f 1 + 2 then 1 else 2")) ^ "\n"));

print_string ("demo: try x with reason" ^ "\n");
print_string ((pretty_print_value (run "try x with reason")) ^ "\n");

print_string ("demo: try raise 12345 with (reason = 9 && error = 12345)" ^ "\n");
print_string ((pretty_print_value (run "try raise 12345 with (reason = 9 && error = 12345)")) ^ "\n");

print_string ("demo: try 1 / 0 with reason = 1" ^ "\n");
print_string ((pretty_print_value (run "try 1 / 0 with reason = 1")) ^ "\n");

print_string ("demo: (1 && 2 * false) 3" ^ "\n");
print_string ((value_to_string (run "(1 && 2 * false) 3")) ^ "\n");

print_string ("demo: fun x -> if x then 1 else 2" ^ "\n");
print_string ((mintype_to_string (get_type ("fun x -> if x then 1 else 2"))) ^ "\n");

print_string ("demo: (fun t -> fun f -> fun x -> if x then t else f) 1 2 true" ^ "\n");
print_string ((pretty_print_value (run "(fun t -> fun f -> fun x -> if x then t else f) 1 2 true")) ^ "\n");

print_string ("demo: (fun t -> fun f -> fun x -> if x then t else f)" ^ "\n");
print_string ((mintype_to_string (get_type ("(fun t -> fun f -> fun x -> if x then t else f)")) ^ "\n"));
