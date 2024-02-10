open Syntax;;

let emptyenv () = Hashtbl.create 10;;

let break_ext env x v = Hashtbl.add env x v; env
let snapshot env = Hashtbl.copy env

let lookup x env =
  try Hashtbl.find env x
  with Not_found -> raise (Failure ("unbound variable: " ^ x));;

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
  | Less(e1, e2) -> "(" ^ (exp_to_string e1) ^ " < " ^ (exp_to_string e2) ^ ")"
  | Greater(e1, e2) -> "(" ^ (exp_to_string e1) ^ " > " ^ (exp_to_string e2) ^ ")"
  | LessEq(e1, e2) -> "(" ^ (exp_to_string e1) ^ " <= " ^ (exp_to_string e2) ^ ")"
  | GreaterEq(e1, e2) -> "(" ^ (exp_to_string e1) ^ " >= " ^ (exp_to_string e2) ^ ")"

  | If(e1, e2, e3) -> "if " ^ (exp_to_string e1) ^ " then " ^ (exp_to_string e2) ^ " else " ^ (exp_to_string e3)
  | Match(e1, l) -> "match " ^ (exp_to_string e1) ^ " with " ^ (String.concat " | " (List.map (fun (e1, e2) -> (exp_to_string e1) ^ " -> " ^ (exp_to_string e2)) l))

  | Var(s) -> s
  | Let(s, e1, e2) -> "let " ^ s ^ " = " ^ (exp_to_string e1) ^ " in " ^ (exp_to_string e2)
  | LetRec(s1, s2, e1, e2) -> "let rec " ^ s1 ^ " = " ^ s2 ^ " in " ^ (exp_to_string e1) ^ " in " ^ (exp_to_string e2)
  | Fun(s, e1) -> "fun " ^ s ^ " -> " ^ (exp_to_string e1)
  | App(e1, e2) -> (exp_to_string e1) ^ "(" ^ (exp_to_string e2) ^ ")"
  | Cons(e1, e2) -> (exp_to_string e1) ^ " :: " ^ (exp_to_string e2)
  | Head(e1) -> "List.hd " ^ (exp_to_string e1)
  | Tail(e1) -> "List.tl " ^ (exp_to_string e1)
  | Empty -> "[]"

let rec eval e env = 
  let eval_env e = eval e env in
  let rec int_binop f second first op = 
    match (eval_env first, eval_env second) with
    | (IntVal(i1), IntVal(i2)) -> IntVal(f i2 i1)
    | (first, second) -> BinOpTypeErr(op, second, first) in
  let rec int_comp f second first op = 
    match (eval_env first, eval_env second) with
    | (IntVal(i1), IntVal(i2)) -> BoolVal(f i2 i1)
    | (first, second) -> BinOpTypeErr(op, second, first) in
  let rec bool_binop f second first op = 
    match (eval_env first, eval_env second) with
    | (BoolVal(b1), BoolVal(b2)) -> BoolVal(f b2 b1)
    | (first, second) -> BinOpTypeErr(op, second, first) in

  match e with
  | IntLit(i) -> IntVal(i)
  | BoolLit(b) -> BoolVal(b)

  | Plus(e1, e2) -> int_binop ( + ) e1 e2 "+"
  | Minus(e1, e2) -> int_binop ( - ) e1 e2 "-"
  | Times(e1, e2) -> int_binop ( * ) e1 e2 "*"
  | Div(e1, e2) -> 
      (match(eval_env e2, eval_env e1) with
      | (IntVal(0), divee) -> DivByZeroErr(divee)
      | (IntVal(i2), IntVal(i1)) -> IntVal(i1 / i2)
      | (a, b) -> BinOpTypeErr("/", b, a))
  | Neg(e1) -> 
    (match (eval_env e1) with
    | IntVal(i) -> IntVal(-i)
    | v -> UnOpTypeErr("-", v))

  | And(e1, e2) -> bool_binop ( && ) e1 e2 "&&"
  | Or(e1, e2) -> bool_binop ( || ) e1 e2 "||"
  | Not(e1) -> 
    (match (eval_env e1) with
    | BoolVal(b) -> BoolVal(not b)
    | v -> UnOpTypeErr("~", v))
  | Eq(e1, e2) -> 
    (match (eval_env e2, eval_env e1) with
    | (IntVal(i2), IntVal(i1)) -> BoolVal(i1 = i2)
    | (BoolVal(b2), BoolVal(b1)) -> BoolVal(b1 = b2)
    | (a, b) -> BinOpTypeErr("=", b, a))
  | Neq(e1, e2) ->
    (match (eval_env e2, eval_env e1) with
    | (IntVal(i2), IntVal(i1)) -> BoolVal(i1 <> i2)
    | (BoolVal(b2), BoolVal(b1)) -> BoolVal(b1 <> b2)
    | (a, b) -> BinOpTypeErr("<>", b, a))
  | Less(e1, e2) -> int_comp ( < ) e1 e2 "<"
  | Greater(e1, e2) -> int_comp ( > ) e1 e2 ">"
  | LessEq(e1, e2) -> int_comp ( <= ) e1 e2 "<="
  | GreaterEq(e1, e2) -> int_comp ( >= ) e1 e2 ">="
  | If(e1, e2, e3) ->
      (match (eval_env e1) with
      | BoolVal(true) -> eval_env e2
      | BoolVal(false) -> eval_env e3
      | v -> IfTypeErr(v))
  | Var(s) -> (lookup (s) env)
  | Let(s, e1, e2) -> (eval e2 (break_ext env s (eval e1 env)))
  (*
  | Cons(e1, e2) -> 
    (match (eval_env e2) with
    | ListVal(l) -> ListVal((eval_env e1) :: l)
    | _ -> failwith "Type error(Cons)")
  | Head(e1) -> 
    (match (eval_env e1) with
    | ListVal(h :: t) -> h
    | _ -> failwith "Type error(Head)")
  | Tail(e1) ->
    (match (eval_env e1) with
    | ListVal(h :: t) -> ListVal(t)
    | _ -> failwith "Type error(Tail)")
  | Empty -> (ListVal([]))
  *)
  | Fun(s, e1) -> FunVal(s, e1, snapshot env)
  | LetRec(func_name, arg, body, expr) -> (eval expr (break_ext env func_name (RecFunVal(func_name, arg, body, snapshot env)))) (* evaludating expr needs recursive function *)
  | App(f, arg) -> 
      let arg = eval arg env in (* before evaluating the function body, evaluate the argument *)
      let f = eval_env f in (* evaluate the function *)
      (match f with
      | FunVal(x, func_body, bind_env) -> 
          eval func_body 
          (break_ext (snapshot bind_env) x arg) (* evaluate the function body with the argument *)
      | RecFunVal(func_name, x, func_body, bind_env) -> 
          eval func_body 
          (break_ext (break_ext (snapshot bind_env) func_name f) x arg) (* evaluating the recursive function needs to bind the function itself *)
      | not_func -> NotAFunctionErr(not_func, arg))
  | _ -> Unimplemented("Not implemented(" ^ (exp_to_string e) ^ ")")

(* TODO
let error_by_to_string by =
  match by with
  | Plus -> "+"
  ...
*)

let rec value_to_string v = 
  match v with
  | IntVal(i) -> string_of_int i
  | BoolVal(b) -> string_of_bool b
  | ListVal(l) -> "[" ^ (String.concat "; " (List.map value_to_string l)) ^ "]"
  | FunVal(arg_name, exp, _env) -> "<fun>(" ^ arg_name ^ ") -> " ^ (exp_to_string exp)
  | RecFunVal(func_name, arg_name, exp, _env) -> func_name ^ "(" ^ arg_name ^ ") -> " ^ (exp_to_string exp)
  | DivByZeroErr(v) -> "Divide by zero{" ^ (value_to_string v) ^ " / 0}"
  | BinOpTypeErr(op, v1, v2) -> "Type error{" ^ (value_to_string v1) ^ " " ^ op ^ " " ^ (value_to_string v2) ^ "}"
  | UnOpTypeErr(msg, v) -> "Type error{" ^ msg ^ " " ^ (value_to_string v) ^ "}"
  | IfTypeErr(v) -> "Type error{If(" ^ (value_to_string v) ^ ")}"
  | NotAFunctionErr(func_body, arg) -> "Not a function{" ^ (value_to_string func_body) ^ "(" ^ (value_to_string arg) ^ ")}"
  | Unimplemented(msg) -> msg
