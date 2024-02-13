open Syntax;;

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
