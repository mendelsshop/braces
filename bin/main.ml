let () = print_endline "Hello, World!"
let x, _ = Braces.Parser.parse_sexpr [ '('; 'a'; '{'; 'a'; '['; 'a'; '['; ')' ]

let _ =
  print_endline
    (match x with
    | `normal x -> Braces.Sexpr.sexpr_to_string x
    | `close _ -> "close"
    | `empty -> "empty")
