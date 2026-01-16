let () = print_endline "Hello, World!"

open Braces.Parser

let x, _ =
  parse_sexpr (fun () -> Error `empty) [ '('; 'a'; '{'; 'a'; '['; 'a'; '[' ]

let _ =
  print_endline
    (match x with
    | `normal x -> Braces.Sexpr.sexpr_to_string x
    | `close _ -> "close"
    | `empty -> "empty")
