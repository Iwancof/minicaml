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

  (* Functions *)
  | Fun of string * exp
  | LetRec of string * string * exp * exp
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
  | DivByZeroErr of value (* value / 0 occurs *)
  | BinOpTypeErr of (error_bin_op * value * value)
  | UnOpTypeErr of (error_un_op * value)
  | IfCondTypeErr of value
  | NotAFunctionErr of value * value
  | UnboundErr of string
  | EmptyListErr
  | Unimplemented of string
and env = (string, value) Hashtbl.t
and mintype = 
  | IntTy
  | BoolTy
  | ListTy of mintype
  | EmptyListTy
  | FunTy of mintype * mintype * env
  | BinOpTypeErr of (error_bin_op * mintype * mintype)
  | UnOpTypeErr of (error_un_op * mintype)
  | IfCondTypeErr of mintype
  | IfArmTypeErr of mintype * mintype
  | NotAFunctionErr of mintype * mintype
  | UnboundErr of string
  | RuntimeError of string
and error_bin_op =
  | OPlus
  | OMinus
  | OTimes
  | ODiv
  | OAnd
  | OOr
  | OEq
  | ONeq
  | OLess
  | OGreater
  | OLessEq
  | OGreaterEq
  | OCons
and error_un_op =
  | ONeg
  | ONot
  | OHead
  | OTail

(* to_string helpers *)

let rec error_bin_op_to_string op =
  match op with
  | OPlus -> "+"
  | OMinus -> "-"
  | OTimes -> "*"
  | ODiv -> "/"
  | OAnd -> "&&"
  | OOr -> "||"
  | OEq -> "="
  | ONeq -> "<>"
  | OLess -> "<"
  | OGreater -> ">"
  | OLessEq -> "<="
  | OGreaterEq -> ">="
  | OCons -> "::"
and error_un_op_to_string op =
  match op with
  | ONeg -> "-"
  | ONot -> "!"
  | OHead -> "List.hd"
  | OTail -> "List.tl"
and value_to_string v = 
  match v with
  | IntVal(i) -> string_of_int i
  | BoolVal(b) -> string_of_bool b
  | ListVal(l) -> "[" ^ (String.concat "; " (List.map value_to_string l)) ^ "]"
  | FunVal(arg_name, exp, _env) -> "<fun>(" ^ arg_name ^ ") -> " ^ (exp_to_string exp)
  | RecFunVal(func_name, arg_name, exp, _env) -> func_name ^ "(" ^ arg_name ^ ") -> " ^ (exp_to_string exp)
  | DivByZeroErr(v) -> "Divide by zero{" ^ (value_to_string v) ^ " / 0}"
  | BinOpTypeErr(op, v1, v2) -> "Type error{" ^ (value_to_string v1) ^ " " ^ (error_bin_op_to_string op) ^ " " ^ (value_to_string v2) ^ "}"
  | UnOpTypeErr(op, v) -> "Type error{" ^ (error_un_op_to_string op) ^ " " ^ (value_to_string v) ^ "}"
  | IfCondTypeErr(v) -> "Type error{If(" ^ (value_to_string v) ^ ")}"
  | NotAFunctionErr(func_body, arg) -> "Not a function{" ^ (value_to_string func_body) ^ "(" ^ (value_to_string arg) ^ ")}"
  | UnboundErr(s) -> "Unbound variable{" ^ s ^ "}"
  | EmptyListErr -> "Empty list{[]}"
  | Unimplemented(msg) -> msg
and exp_to_string e = 
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
and mintype_to_string ty = 
  match ty with
  | IntTy -> "int"
  | BoolTy -> "bool"
  | ListTy(ity) -> mintype_to_string ity ^ " list"
  | EmptyListTy -> "(not determined) list"
  | FunTy(arg, ret, _env) -> "fun " ^ mintype_to_string arg ^ " -> " ^ mintype_to_string ret
  | BinOpTypeErr(op, left, right) -> "Type error{" ^ (mintype_to_string left) ^ " " ^ (error_bin_op_to_string op) ^ " " ^ (mintype_to_string right) ^ "}"
  | UnOpTypeErr(op, v) -> "Type error{" ^ (error_un_op_to_string op) ^ " " ^ (mintype_to_string v) ^ "}"
  | IfCondTypeErr(v) -> "Type error{If(" ^ (mintype_to_string v) ^ ")}"
  | IfArmTypeErr(t, f) -> "Type error{If cond then (" ^ (mintype_to_string t) ^ ") else (" ^ (mintype_to_string f) ^ ")"
  | NotAFunctionErr(func_body, arg) -> "Not a function{" ^ (mintype_to_string func_body) ^ "(" ^ (mintype_to_string arg) ^ ")}"
  | UnboundErr(s) -> "Unbound variable{" ^ s ^ "}"
  | RuntimeError(msg) -> "Runtime type error{" ^ msg ^ "}"
(*
and value_to_type value =
  match value with
  | IntVal(v) -> IntTy
  | BoolVal(b) -> BoolTy
  | ListVal(l) -> failwith "Unimplemented"
  | FunVal(_arg, _exp, _env) -> failwith "unimplemented"
  | RecFunVal(_name, _arg, exp, _env) -> failwith "unimplemented"
  | BinOpTypeErr(op, left, right) -> BinOpTypeErr(op, value_to_type left, value_to_type right)
  | UnOpTypeErr(op, v) -> UnOpTypeErr(op, value_to_type v)
  | IfCondTypeErr(v) -> IfCondTypeErr(value_to_type v)
  | NotAFunctionErr(func, arg) -> NotAFunctionErr(value_to_type func, value_to_type arg)
  | UnboundErr(x) -> UnboundErr(x)
  | EmptyListErr -> RuntimeError("EmptyList")
  | DivByZeroErr(v) -> RuntimeError("DivByZeroErr")
  | Unimplemented(msg) -> RuntimeError("Unimplemented: " ^ msg)
*)
