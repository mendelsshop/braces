type sexpr = Symbol of string | List of sexpr list | Number of int

let rec sexpr_to_string = function
  | Number i -> Int.to_string i
  | Symbol s -> s
  | List s -> "(" ^ (List.map sexpr_to_string s |> String.concat " ") ^ ")"
