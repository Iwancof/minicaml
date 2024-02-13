open Syntax;;

let break_ext env x v = Hashtbl.add env x v; env
let snapshot env = Hashtbl.copy env

type mintype = 
  | VarTy of string
  | IntTy
  | BoolTy
  | ListTy of mintype
  | EmptyListTy
  | ArrowTy of mintype * mintype
  | BinOpTypeErr of (error_bin_op * mintype * mintype)
  | UnOpTypeErr of (error_un_op * mintype)
  | IfCondTypeErr of mintype
  | IfArmTypeErr of mintype * mintype
  | NotAFunctionErr of mintype * mintype
  | ArgumentErr of mintype * mintype
  | UnboundErr of string
  | RuntimeError of string

let rec mintype_to_string ty = 
  match ty with
  | VarTy(s) -> s
  | IntTy -> "int"
  | BoolTy -> "bool"
  | ListTy(ity) -> mintype_to_string ity ^ " list"
  | EmptyListTy -> "(not determined) list"
  | ArrowTy(arg, ret) -> "fun " ^ mintype_to_string arg ^ " -> " ^ mintype_to_string ret
  | BinOpTypeErr(op, left, right) -> "Type error{" ^ (mintype_to_string left) ^ " " ^ (error_bin_op_to_string op) ^ " " ^ (mintype_to_string right) ^ "}"
  | UnOpTypeErr(op, v) -> "Type error{" ^ (error_un_op_to_string op) ^ " " ^ (mintype_to_string v) ^ "}"
  | IfCondTypeErr(v) -> "Type error{If(" ^ (mintype_to_string v) ^ ")}"
  | IfArmTypeErr(t, f) -> "Type error{If cond then (" ^ (mintype_to_string t) ^ ") else (" ^ (mintype_to_string f) ^ ")"
  | NotAFunctionErr(func_body, arg) -> "Not a function{" ^ (mintype_to_string func_body) ^ "(" ^ (mintype_to_string arg) ^ ")}"
  | ArgumentErr(expected, actual) -> "Type error{Expected " ^ (mintype_to_string expected) ^ " but got " ^ (mintype_to_string actual) ^ "}"
  | UnboundErr(s) -> "Unbound variable{" ^ s ^ "}"
  | RuntimeError(msg) -> "Runtime type error{" ^ msg ^ "}"

let temporary_type_var s = s ^ "'"
let new_typevar s = VarTy(temporary_type_var s)

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

