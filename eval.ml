open Syntax;;
open Type;;

let emptyenv () = Hashtbl.create 10;;

let break_ext env x v = Hashtbl.add env x v; env
let snapshot env = Hashtbl.copy env

let lookup x env: value =
  try Hashtbl.find env x
  with Not_found -> UnboundErr(x)

let lookup_type x env: mintype =
  try Hashtbl.find env x
  with Not_found -> UnboundErr(x)

let rec typeof e env =
  let typeof_inner e = typeof e env in
  match e with
  | IntLit(_) -> IntTy
  | BoolLit(_) -> BoolTy

  | Plus(left, right) -> (match (typeof_inner left, typeof_inner right) with
    | (IntTy, IntTy) -> IntTy
    | (l, r) -> BinOpTypeErr(OPlus, l, r))
  | Minus(left, right) -> (match (typeof_inner left, typeof_inner right) with
    | (IntTy, IntTy) -> IntTy
    | (l, r) -> BinOpTypeErr(OMinus, l, r))
  | Times(left, right) -> (match (typeof_inner left, typeof_inner right) with
    | (IntTy, IntTy) -> IntTy
    | (l, r) -> BinOpTypeErr(OTimes, l, r))
  | Div(left, right) -> (match (typeof_inner left, typeof_inner right) with
    | (IntTy, IntTy) -> IntTy
    | (l, r) -> BinOpTypeErr(ODiv, l, r))
  | Neg(v) -> (match (typeof_inner v) with
    | IntTy -> IntTy
    | v -> UnOpTypeErr(ONeg, v))
  | And(left, right) -> (match (typeof_inner left, typeof_inner right) with
    | (BoolTy, BoolTy) -> BoolTy
    | (l, r) -> BinOpTypeErr(OAnd, l, r))
  | Or(left, right) -> (match (typeof_inner left, typeof_inner right) with
    | (BoolTy, BoolTy) -> BoolTy
    | (l, r) -> BinOpTypeErr(OOr, l, r))
  | Not(v) -> (match (typeof_inner v) with
    | BoolTy -> BoolTy
    | v -> UnOpTypeErr(ONot, v))
  | Eq(left, right) -> (match (typeof_inner left, typeof_inner right) with
    | (IntTy, IntTy) -> BoolTy
    | (BoolTy, BoolTy) -> BoolTy
    | (ListTy(l), ListTy(r)) -> (
      if l = r then
        BoolTy
      else
        BinOpTypeErr(OEq, l, r) (* Error occured in list *)
    )
    | (EmptyListTy, EmptyListTy) -> BoolTy
    | (l, r) -> BinOpTypeErr(OEq, l, r))
  | Neq(left, right) -> (match (typeof_inner left, typeof_inner right) with
    | (IntTy, IntTy) -> BoolTy
    | (BoolTy, BoolTy) -> BoolTy
    | (ListTy(l), ListTy(r)) -> (
      if l = r then
        BoolTy
      else
        BinOpTypeErr(ONeq, l, r) (* Error occured in list *)
    )
    | (EmptyListTy, EmptyListTy) -> BoolTy
    | (l, r) -> BinOpTypeErr(ONeq, l, r))
  | Less(left, right) -> (match (typeof_inner left, typeof_inner right) with
    | (IntTy, IntTy) -> BoolTy
    | (l, r) -> BinOpTypeErr(OLess, l, r))
  | Greater(left, right) -> (match (typeof_inner left, typeof_inner right) with
    | (IntTy, IntTy) -> BoolTy
    | (l, r) -> BinOpTypeErr(OGreater, l, r))
  | LessEq(left, right) -> (match (typeof_inner left, typeof_inner right) with
    | (IntTy, IntTy) -> BoolTy
    | (l, r) -> BinOpTypeErr(OLessEq, l, r))
  | GreaterEq(left, right) -> (match (typeof_inner left, typeof_inner right) with
    | (IntTy, IntTy) -> BoolTy
    | (l, r) -> BinOpTypeErr(OGreaterEq, l, r))
  | If(cond, t, f) -> (match (typeof_inner cond) with
      | BoolTy -> 
          (
          let tt = typeof_inner t in
          let ft = typeof_inner f in
          if tt = ft then
            tt
          else
            IfArmTypeErr(tt, ft)
          )
      | other -> IfCondTypeErr(other))
  | Var(s) -> (lookup_type s env)
  | Let(s, e1, e2) -> (typeof e2 (break_ext env s (typeof_inner e1)))
  | Cons(e1, e2) ->
      (
        let list_type = typeof_inner e2 in
        let elm_type = typeof_inner e1 in
        match list_type with
        | ListTy(inner_type) -> (
          if elm_type = inner_type then
            ListTy(inner_type)
          else
            BinOpTypeErr(OCons, elm_type, list_type)
          )
        | EmptyListTy ->  ListTy(elm_type)
        | other -> BinOpTypeErr(OCons, elm_type, other)
      )
  | Head(l) ->
      (
        match (typeof_inner l) with
        | ListTy(t) -> t
        | EmptyListTy -> RuntimeError("List is empty")
        | other -> UnOpTypeErr(OHead, other)
      )
  | Tail(l) ->
      (
        match (typeof_inner l) with
        | ListTy(t) -> ListTy(t)
        | EmptyListTy -> RuntimeError("List is empty")
        | other -> UnOpTypeErr(OTail, other)
      )
  | Empty -> EmptyListTy
  | Fun(arg, exp) -> failwith "Unimplemented"
  | LetRec(fname, arg, body, exp) -> failwith "Unimplemented"
  | App(f, arg) -> failwith "Unimplemented"
  | _ -> failwith "Unimplemented"

let rec eval e env = 
  let eval_env e = eval e env in
  let rec int_binop f second first op = 
    match (eval_env first, eval_env second) with
    | (IntVal(i1), IntVal(i2)) -> IntVal(f i2 i1)
    | (first, second) -> (BinOpTypeErr(op, second, first)) in
  let rec int_comp f second first op = 
    match (eval_env first, eval_env second) with
    | (IntVal(i1), IntVal(i2)) -> BoolVal(f i2 i1)
    | (first, second) -> (BinOpTypeErr(op, second, first)) in
  let rec bool_binop f second first op = 
    match (eval_env first, eval_env second) with
    | (BoolVal(b1), BoolVal(b2)) -> BoolVal(f b2 b1)
    | (first, second) -> (BinOpTypeErr(op, second, first)) in

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
      | (a, b) -> (BinOpTypeErr(ODiv, b, a)))
  | Neg(e1) -> 
    (match (eval_env e1) with
    | IntVal(i) -> IntVal(-i)
    | v -> (UnOpTypeErr(ONeg, v)))

  | And(e1, e2) -> bool_binop ( && ) e1 e2 OAnd
  | Or(e1, e2) -> bool_binop ( || ) e1 e2 OOr
  | Not(e1) -> 
    (match (eval_env e1) with
    | BoolVal(b) -> BoolVal(not b)
    | v -> (UnOpTypeErr(ONot, v)))
  | Eq(e1, e2) -> 
    (match (eval_env e2, eval_env e1) with
    | (IntVal(i2), IntVal(i1)) -> BoolVal(i1 = i2)
    | (BoolVal(b2), BoolVal(b1)) -> BoolVal(b1 = b2)
    | (ListVal(l2), ListVal(l1)) -> BoolVal(l1 = l2)
    | (a, b) -> (BinOpTypeErr(OEq, b, a)))
  | Neq(e1, e2) ->
    (match (eval_env e2, eval_env e1) with
    | (IntVal(i2), IntVal(i1)) -> BoolVal(i1 <> i2)
    | (BoolVal(b2), BoolVal(b1)) -> BoolVal(b1 <> b2)
    | (ListVal(l2), ListVal(l1)) -> BoolVal(l1 <> l2)
    | (a, b) -> (BinOpTypeErr(ONeq, b, a)))
  | Less(e1, e2) -> int_comp ( < ) e1 e2 OLess
  | Greater(e1, e2) -> int_comp ( > ) e1 e2 OGreater
  | LessEq(e1, e2) -> int_comp ( <= ) e1 e2 OLessEq
  | GreaterEq(e1, e2) -> int_comp ( >= ) e1 e2 OGreaterEq
  | If(e1, e2, e3) ->
      (match (eval_env e1) with
      | BoolVal(true) -> eval_env e2
      | BoolVal(false) -> eval_env e3
      | v -> (IfCondTypeErr(v)))
  | Var(s) -> (lookup (s) env)
  | Let(s, e1, e2) -> (eval e2 (break_ext env s (eval e1 env)))
  | Cons(e1, e2) -> 
    (match (eval_env e2) with
    | ListVal(l) -> ListVal((eval_env e1) :: l)
    | v -> (BinOpTypeErr(OCons, eval_env e1, v)))
  | Head(e1) -> 
    (match (eval_env e1) with
    | ListVal(h :: t) -> h
    | ListVal([]) -> EmptyListErr
    | v -> (UnOpTypeErr(OHead, v)))
  | Tail(e1) ->
    (match (eval_env e1) with
    | ListVal(h :: t) -> ListVal(t)
    | ListVal([]) -> EmptyListErr
    | v -> (UnOpTypeErr(OTail, v)))
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
      | not_func -> (NotAFunctionErr(not_func, arg)))
  | _ -> Unimplemented("Not implemented(" ^ (exp_to_string e) ^ ")")

