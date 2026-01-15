type sexpr = Symbol of string | List of sexpr list

let rec sexpr_to_string = function
  | Symbol s -> s
  | List s -> "(" ^ (List.map sexpr_to_string s |> String.concat " ") ^ ")"
