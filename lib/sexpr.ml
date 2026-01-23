type sexpr =
  | Symbol of string
  | List of sexpr list
  | Number of int
  | Boolean of bool

let rec sexpr_to_string = function
  | Number i -> Int.to_string i
  | Boolean b -> string_of_bool b
  | Symbol s -> s
  | List s -> "(" ^ (List.map sexpr_to_string s |> String.concat " ") ^ ")"
