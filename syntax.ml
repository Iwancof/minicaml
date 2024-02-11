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
  | BinOpTypeErr of (string * value * value)
  | UnOpTypeErr of (string * value)
  | IfTypeErr of value
  | EmptyListErr
  | NotAFunctionErr of value * value
  | Unimplemented of string
and env = (string, value) Hashtbl.t

