(* main.ml *)

(* 構文定義ファイル syntax.ml で定義された exp型を使う *)
open Syntax ;;
open Eval ;;
open Type ;;

(* 与えられた文字列の字句解析と構文解析だけを行う関数 *)
(* parse : string -> exp *)

let parse str = 
  Myparser.main Mylexer.token 
    (Lexing.from_string str)

let dump str = 
  let e = parse str in
  exp_to_string e;;

let run str = 
  let e = parse str in
  Eval.eval e (Eval.emptyenv ());;

let get_type str = 
  let e = parse str in
  typeof (parse str) (emptyenv ());;

(* 使用例は以下の通り。parse関数は Mainモジュールにはいっているので
   open Main;; parse "...";; とするか Main.parse "...";;
   として呼びだす。

$ ./miniocaml
      Objective Caml version 3.09.1

# Main.parse "let x = 3 + 1 * 4 in fun y -> x + y";;
- : Syntax.exp =
Syntax.Let ("x",
 Syntax.Plus (Syntax.IntLit 3,
  Syntax.Times (Syntax.IntLit 1, Syntax.IntLit 4)),
 Syntax.Fun ("y", Syntax.Plus (Syntax.Var "x", Syntax.Var "y")))
#  Main.parse "1 + 2 * 3 ";;
- : Syntax.exp =
Syntax.Plus (Syntax.IntLit 1,
 Syntax.Times (Syntax.IntLit 2, Syntax.IntLit 3))
#
*)
