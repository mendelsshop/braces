open Sexpr
module Env = Map.Make (String)

type eval_sexpr = Lambda of (eval_sexpr list -> eval_sexpr) | Number of int

let rec eval env = function
  | List (Symbol "lambda" :: arguments) -> failwith ""
  | List (Symbol "if" :: arguments) -> failwith ""
  | List (lambda :: arguments) -> failwith ""
  | Number i -> Number i
  | Symbol s -> Env.find s env
