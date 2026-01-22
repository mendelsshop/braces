open Sexpr
module Env = Map.Make (String)

type eval_sexpr =
  | Lambda of (eval_sexpr -> eval_sexpr)
  | Number of int
  | Boolean of bool

let rec eval env = function
  | List [ Symbol "lambda"; Symbol parameter; body ] ->
      Lambda (fun arg -> eval (Env.add parameter arg env) body)
  | List (Symbol "lambda" :: _) -> failwith "bad form lambda"
  | List [ Symbol "if"; cond; cons; alt ] ->
      if eval env cond = Boolean false then eval env alt else eval env cons
  | List (Symbol "if" :: _) -> failwith "bad form if"
  (* application is like ml application i.e (f a b c) => (((f a) b) c) *)
  (* also no such thing as (f) *)
  | List (lambda :: arguments) ->
      let lambda = eval env lambda in
      List.fold_left
        (function
          | Lambda lambda -> fun arg -> lambda (eval env arg)
          | _ -> failwith "application to non lambda")
        lambda arguments
  | List _ -> failwith "bad form empty list"
  | Number i -> Number i
  | Symbol s -> Env.find s env
