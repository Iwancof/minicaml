open Syntax;;

type mintype = 
  | IntTy
  | BoolTy
  | ListTy of mintype
  | EmptyListTy
  | FunTy of mintype * mintype
  | BinOpTypeErr of (error_bin_op * mintype * mintype)
  | UnOpTypeErr of (error_un_op * mintype)
  | IfCondTypeErr of mintype
  | IfArmTypeErr of mintype * mintype
  | NotAFunctionErr of mintype * mintype
  | UnboundErr of string
  | RuntimeError of string

let rec mintype_to_string ty = 
  match ty with
  | IntTy -> "int"
  | BoolTy -> "bool"
  | ListTy(ity) -> mintype_to_string ity ^ " list"
  | EmptyListTy -> "(not determined) list"
  | FunTy(arg, ret) -> "fun " ^ mintype_to_string arg ^ " -> " ^ mintype_to_string ret
  | BinOpTypeErr(op, left, right) -> "Type error{" ^ (mintype_to_string left) ^ " " ^ (error_bin_op_to_string op) ^ " " ^ (mintype_to_string right) ^ "}"
  | UnOpTypeErr(op, v) -> "Type error{" ^ (error_un_op_to_string op) ^ " " ^ (mintype_to_string v) ^ "}"
  | IfCondTypeErr(v) -> "Type error{If(" ^ (mintype_to_string v) ^ ")}"
  | IfArmTypeErr(t, f) -> "Type error{If cond then (" ^ (mintype_to_string t) ^ ") else (" ^ (mintype_to_string f) ^ ")"
  | NotAFunctionErr(func_body, arg) -> "Not a function{" ^ (mintype_to_string func_body) ^ "(" ^ (mintype_to_string arg) ^ ")}"
  | UnboundErr(s) -> "Unbound variable{" ^ s ^ "}"
  | RuntimeError(msg) -> "Runtime type error{" ^ msg ^ "}"
