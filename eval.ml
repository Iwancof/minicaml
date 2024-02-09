let emptyenv() = [];;

let ext env x v = (x, v) :: env;;

let rec lookup x env = match env with
  | [] -> raise (Failure ("unbound variable: " ^ x))
  | (y, v) :: rest -> if x = y then v else lookup x rest;;


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
  | Less of exp * exp
  | Greater of exp * exp
  | LessEq of exp * exp
  | GreaterEq of exp * exp

  (* Control flow *)
  | If of exp * exp * exp
  | Match of exp * ((exp * exp) list)

  (* Variables *)
  | Var of string
  | Let of string * exp * exp
  | LetRec of string * string * exp * exp

  (* Functions *)
  | Fun of string * exp
  | App of exp * exp

  (* Lists *)
  | Cons of exp * exp
  | Head of exp
  | Tail of exp
  | Empty

type value = 
  | IntVal of int
  | BoolVal of bool
  | ListVal of value list
  | FunVal of string * exp * env
  | RecFunVal of string * string * exp * env
and env = (string * value) list

let rec value_to_string v = 
  match v with
  | IntVal(i) -> string_of_int i
  | BoolVal(b) -> string_of_bool b
  | ListVal(l) -> "[" ^ (String.concat "; " (List.map value_to_string l)) ^ "]"
  | FunVal(_, _, _) -> "<fun>"
  | RecFunVal(_, _, _, _) -> "<fun>"

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
  let expect_int v = 
    match v with
    | IntVal(i) -> i
    | _ -> failwith "Type error(expect_int)" in
  let expect_bool v = 
    match v with
    | BoolVal(b) -> b
    | _ -> failwith "Type error(expect_bool)" in
  let rec int_binop f e1 e2 = IntVal(f (expect_int (eval_env e1)) (expect_int (eval_env e2))) in
  let rec int_comp f e1 e2 = BoolVal(f (expect_int (eval_env e1)) (expect_int (eval_env e2))) in
  let rec bool_binop f e1 e2 = BoolVal(f (expect_bool (eval_env e1)) (expect_bool (eval_env e2))) in

  match e with
  | IntLit(i) -> IntVal(i)
  | BoolLit(b) -> BoolVal(b)

  | Plus(e1, e2) -> int_binop ( + ) e1 e2
  | Minus(e1, e2) -> int_binop ( - ) e1 e2
  | Times(e1, e2) -> int_binop ( * ) e1 e2
  | Div(e1, e2) -> int_binop ( / ) e1 e2
  | Neg(e1) -> IntVal(- (expect_int (eval_env e1)))

  | And(e1, e2) -> bool_binop ( && ) e1 e2
  | Or(e1, e2) -> bool_binop ( || ) e1 e2
  | Not(e1) -> BoolVal(not (expect_bool (eval_env e1)))

  | Eq(e1, e2) -> 
    (match (eval_env e1, eval_env e2) with
    | (IntVal(i1), IntVal(i2)) -> BoolVal(i1 = i2)
    | (BoolVal(b1), BoolVal(b2)) -> BoolVal(b1 = b2)
    | _ -> failwith "Type error(Eq)")
  | Neq(e1, e2) ->
    (match (eval_env e1, eval_env e2) with
    | (IntVal(i1), IntVal(i2)) -> BoolVal(i1 <> i2)
    | (BoolVal(b1), BoolVal(b2)) -> BoolVal(b1 <> b2)
    | _ -> failwith "Type error(Neq)")
  | Less(e1, e2) -> int_comp ( < ) e1 e2
  | Greater(e1, e2) -> int_comp ( > ) e1 e2
  | LessEq(e1, e2) -> int_comp ( <= ) e1 e2
  | GreaterEq(e1, e2) -> int_comp ( >= ) e1 e2
  | If(e1, e2, e3) ->
      (match (eval_env e1) with
      | BoolVal(true) -> eval_env e2
      | BoolVal(false) -> eval_env e3
      | _ -> failwith "Type error(If)")
  | Var(s) -> (lookup (s) env)
  | Let(s, e1, e2) -> (eval e2 (ext env s (eval e1 env)))
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
  | _ -> failwith "Not implemented"
