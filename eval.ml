open Syntax;;

let emptyenv () = Hashtbl.create 10;;

let break_ext env x v = Hashtbl.add env x v; env
let snapshot env = Hashtbl.copy env

let lookup x env =
  try Hashtbl.find env x
  with Not_found -> TypeErr(UnboundErr(x))

(*
let rec type_of e =
  match e with
  | IntLit(_) -> IntTy
  | BoolLit(_) -> BoolTy

  | Plus(e1, e2) | Minus(e1, e2) | Times(e1, e2) | Div(e1, e2) -> 
      (match (type_of e1, type_of e2) with
      | (IntTy, IntTy) -> IntTy
      | (t1, t2) -> BinOpTypeErr("+", t1, t2))
*)

let rec eval e env = 
  let eval_env e = eval e env in
  let rec int_binop f second first op = 
    match (eval_env first, eval_env second) with
    | (IntVal(i1), IntVal(i2)) -> IntVal(f i2 i1)
    | (first, second) -> TypeErr(BinOpTypeErr(op, second, first)) in
  let rec int_comp f second first op = 
    match (eval_env first, eval_env second) with
    | (IntVal(i1), IntVal(i2)) -> BoolVal(f i2 i1)
    | (first, second) -> TypeErr(BinOpTypeErr(op, second, first)) in
  let rec bool_binop f second first op = 
    match (eval_env first, eval_env second) with
    | (BoolVal(b1), BoolVal(b2)) -> BoolVal(f b2 b1)
    | (first, second) -> TypeErr(BinOpTypeErr(op, second, first)) in

  match e with
  | IntLit(i) -> IntVal(i)
  | BoolLit(b) -> BoolVal(b)

  | Plus(e1, e2) -> int_binop ( + ) e1 e2 OPlus
  | Minus(e1, e2) -> int_binop ( - ) e1 e2 OMinus
  | Times(e1, e2) -> int_binop ( * ) e1 e2 OTimes
  | Div(e1, e2) -> 
      (match(eval_env e2, eval_env e1) with
      | (IntVal(0), divee) -> DivByZeroErr(divee)
      | (IntVal(i2), IntVal(i1)) -> IntVal(i1 / i2)
      | (a, b) -> TypeErr(BinOpTypeErr(ODiv, b, a)))
  | Neg(e1) -> 
    (match (eval_env e1) with
    | IntVal(i) -> IntVal(-i)
    | v -> TypeErr(UnOpTypeErr(ONeg, v)))

  | And(e1, e2) -> bool_binop ( && ) e1 e2 OAnd
  | Or(e1, e2) -> bool_binop ( || ) e1 e2 OOr
  | Not(e1) -> 
    (match (eval_env e1) with
    | BoolVal(b) -> BoolVal(not b)
    | v -> TypeErr(UnOpTypeErr(ONot, v)))
  | Eq(e1, e2) -> 
    (match (eval_env e2, eval_env e1) with
    | (IntVal(i2), IntVal(i1)) -> BoolVal(i1 = i2)
    | (BoolVal(b2), BoolVal(b1)) -> BoolVal(b1 = b2)
    | (ListVal(l2), ListVal(l1)) -> BoolVal(l1 = l2)
    | (a, b) -> TypeErr(BinOpTypeErr(OEq, b, a)))
  | Neq(e1, e2) ->
    (match (eval_env e2, eval_env e1) with
    | (IntVal(i2), IntVal(i1)) -> BoolVal(i1 <> i2)
    | (BoolVal(b2), BoolVal(b1)) -> BoolVal(b1 <> b2)
    | (a, b) -> TypeErr(BinOpTypeErr(ONeq, b, a)))
  | Less(e1, e2) -> int_comp ( < ) e1 e2 OLess
  | Greater(e1, e2) -> int_comp ( > ) e1 e2 OGreater
  | LessEq(e1, e2) -> int_comp ( <= ) e1 e2 OLessEq
  | GreaterEq(e1, e2) -> int_comp ( >= ) e1 e2 OGreaterEq
  | If(e1, e2, e3) ->
      (match (eval_env e1) with
      | BoolVal(true) -> eval_env e2
      | BoolVal(false) -> eval_env e3
      | v -> TypeErr(IfTypeErr(v)))
  | Var(s) -> (lookup (s) env)
  | Let(s, e1, e2) -> (eval e2 (break_ext env s (eval e1 env)))
  | Cons(e1, e2) -> 
    (match (eval_env e2) with
    | ListVal(l) -> ListVal((eval_env e1) :: l)
    | v -> TypeErr(BinOpTypeErr(OCons, eval_env e1, v)))
  | Head(e1) -> 
    (match (eval_env e1) with
    | ListVal(h :: t) -> h
    | ListVal([]) -> EmptyListErr
    | v -> TypeErr(UnOpTypeErr(OHead, v)))
  | Tail(e1) ->
    (match (eval_env e1) with
    | ListVal(h :: t) -> ListVal(t)
    | ListVal([]) -> EmptyListErr
    | v -> TypeErr(UnOpTypeErr(OTail, v)))
  | Empty -> (ListVal([]))
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
      | not_func -> TypeErr(NotAFunctionErr(not_func, arg)))
  | _ -> Unimplemented("Not implemented(" ^ (exp_to_string e) ^ ")")

