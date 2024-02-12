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

(* TODO
type error_by = 
  | Plus
  | Minus
  ...
*)

type value = 
  | IntVal of int
  | BoolVal of bool
  | ListVal of value list
  | FunVal of string * exp * env
  | RecFunVal of string * string * exp * env
  | DivByZeroErr of value (* value / 0 occurs *)
  | BinOpTypeErr of (error_bin_op * value * value)
  | UnOpTypeErr of (error_un_op * value)
  | IfTypeErr of value
  | NotAFunctionErr of value * value
  | UnboundErr of string
  | EmptyListErr
  | Unimplemented of string
and env = (string, value) Hashtbl.t
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
and ty = 
  | IntTy
  | BoolTy
  | ListTy of ty
  | FunTy of ty * ty
  (* TODO *)

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
  | IfTypeErr(v) -> "Type error{If(" ^ (value_to_string v) ^ ")}"
  | NotAFunctionErr(func_body, arg) -> "Not a function{" ^ (value_to_string func_body) ^ "(" ^ (value_to_string arg) ^ ")}"
  | UnboundErr(s) -> "Unbound variable{" ^ s ^ "}"
  | EmptyListErr -> "Empty list{[]}"
  | Unimplemented(msg) -> msg
and type_to_string t =
  match t with
  | IntTy -> "int"
  | BoolTy -> "bool"
  | ListTy(t) -> (type_to_string t) ^ " list"
  | FunTy(t1, t2) -> (type_to_string t1) ^ " -> " ^ (type_to_string t2)
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
