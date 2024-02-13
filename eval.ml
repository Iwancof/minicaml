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

let rec solve_type x env: mintype =
  match x with
  | VarTy(s) -> lookup_type s env
  | IntTy | BoolTy | EmptyListTy -> x
  | ListTy(t) -> ListTy(solve_type t env)
  | ArrowTy(arg, ret) -> ArrowTy(solve_type arg env, solve_type ret env)
  | BinOpTypeErr(op, l, r) -> BinOpTypeErr(op, solve_type l env, solve_type r env)
  | UnOpTypeErr(op, v) -> UnOpTypeErr(op, solve_type v env)
  | IfCondTypeErr(v) -> IfCondTypeErr(solve_type v env)
  | IfArmTypeErr(t, f) -> IfArmTypeErr(solve_type t env, solve_type f env)
  | NotAFunctionErr(f, a) -> NotAFunctionErr(solve_type f env, solve_type a env)
  | ArgumentErr(e, a) -> ArgumentErr(solve_type e env, solve_type a env)
  | UnboundErr(s) -> UnboundErr(s)
  | RuntimeError(s) -> RuntimeError(s)

let rec typeof e env =
  let typeof_inner e = typeof e env in
  let expect e t = (
    let e_type = typeof_inner e in
    if e_type = t then
      e_type
    else
      match e_type with
      | VarTy(s) -> ignore(break_ext env s t); t
      | other -> other
  ) in
  let eq_helper left right op = (
    let check l r = match (l, r) with
    | (IntTy, IntTy) -> BoolTy
    | (BoolTy, BoolTy) -> BoolTy
    | (ListTy(l), ListTy(r)) -> (
      if l = r then
        BoolTy
      else
        BinOpTypeErr(op, l, r) (* Error occured in list *)
    )
    | (EmptyListTy, EmptyListTy) -> BoolTy
    | (l, r) -> BinOpTypeErr(op, l, r) in
    match (typeof_inner left, typeof_inner right) with
    | (VarTy(s), r) -> check (expect left r) r
    | (l, VarTy(s)) -> check l (expect right l)
    | (l, r) -> check l r
  ) in
  let int_binop_helper left right ret op = (
    match (expect left IntTy, expect right IntTy) with
    | (IntTy, IntTy) -> ret
    | (l, r) -> BinOpTypeErr(op, l, r)
  ) in
  let bool_binop_helper left right ret op = (
    match (expect left BoolTy, expect right BoolTy) with
    | (BoolTy, BoolTy) -> ret
    | (l, r) -> BinOpTypeErr(op, l, r)
  ) in
  match e with
  | IntLit(_) -> IntTy
  | BoolLit(_) -> BoolTy

  | Plus(left, right) -> int_binop_helper left right IntTy OPlus
  | Minus(left, right) -> int_binop_helper left right IntTy OMinus
  | Times(left, right) -> int_binop_helper left right IntTy OTimes
  | Div(left, right) -> int_binop_helper left right IntTy ODiv
  | Neg(v) -> (match (expect v IntTy) with
    | IntTy -> IntTy
    | v -> UnOpTypeErr(ONeg, v))
  | And(left, right) -> bool_binop_helper left right BoolTy OAnd
  | Or(left, right) -> bool_binop_helper left right BoolTy OOr
  | Not(v) -> (match (expect v BoolTy) with
    | BoolTy -> BoolTy
    | v -> UnOpTypeErr(ONot, v))
  | Eq(left, right) -> eq_helper left right OEq
  | Neq(left, right) -> eq_helper left right ONeq
  | Less(left, right) -> int_binop_helper left right BoolTy OLess
  | Greater(left, right) -> int_binop_helper left right BoolTy OGreater
  | LessEq(left, right) -> int_binop_helper left right BoolTy OLessEq
  | GreaterEq(left, right) -> int_binop_helper left right BoolTy OGreaterEq
  | If(cond, t, f) -> (match (expect cond BoolTy) with
      | BoolTy -> 
          (
          let tt = typeof_inner t in
          let ft = typeof_inner f in
          if tt = ft then
            tt
          else
            match (tt, ft) with
            | (VarTy(s), VarTy(r)) -> failwith "The case of both are VarTy is not implemented yet"
            | (VarTy(s), r) -> ignore(break_ext env s r); r
            | (l, VarTy(s)) -> ignore(break_ext env s l); l
            | (t, f) -> IfArmTypeErr(t, f)
          )
      | other -> IfCondTypeErr(other))
  | Var(s) -> (lookup_type s env)
  | Let(s, e1, e2) -> (typeof e2 (break_ext env s (typeof_inner e1)))

  (* Type inference for list is not implemented yet *)

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

  (* ... *)

  | Fun(arg, exp) -> (
    ignore(break_ext env arg (new_typevar arg));
    let exp_type = typeof_inner exp in
    let arg_type = lookup_type arg env in
    ArrowTy(arg_type, exp_type)
  )
  | LetRec(fname, arg, body, follow) -> (
    let arg = let n = new_typevar arg in ignore(break_ext env arg n); n in
    let func_type = ArrowTy(arg, typeof_inner body) in
    let env = break_ext env fname func_type in
    typeof follow env
  )
  | App(f, arg) -> (
    let f_type = typeof_inner f in
    let arg_type = typeof_inner arg in
    match f_type with
    | ArrowTy(VarTy(s), ret) -> (
      ignore(break_ext env s arg_type);
      solve_type ret env
    )
    | ArrowTy(expect, ret) -> (
      if expect = arg_type then
        ret
      else
        ArgumentErr(expect, arg_type)
    )
    | other -> NotAFunctionErr(other, arg_type)
  )
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
  | Try(t, c) -> (
      let t = eval_env t in
      (match t with
      | DivByZeroErr(_) -> (ignore(break_ext env "reason" (IntVal 1)); eval_env c)
      | BinOpTypeErr(_, _, _) -> (ignore(break_ext env "reason" (IntVal 2)); eval_env c)
      | UnOpTypeErr(_, _) -> (ignore(break_ext env "reason" (IntVal 3)); eval_env c)
      | IfCondTypeErr(_) -> (ignore(break_ext env "reason" (IntVal 4)); eval_env c)
      | NotAFunctionErr(_, _) -> (ignore(break_ext env "reason" (IntVal 5)); eval_env c)
      | UnboundErr(_) -> (ignore(break_ext env "reason" (IntVal 6)); eval_env c)
      | EmptyListErr -> (ignore(break_ext env "reason" (IntVal 7)); eval_env c)
      | Unimplemented(_) -> (ignore(break_ext env "reason" (IntVal 8)); eval_env c)
      | RuntimeErr(r) -> (ignore(break_ext env "reason" (IntVal 9)); ignore(break_ext env "error" r); eval_env c)
      | _ -> t)
    )
  | Raise(r) -> RuntimeErr(eval_env r)
  | _ -> failwith "Unimplemented"
