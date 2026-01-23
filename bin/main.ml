let () = print_endline "Hello, World!"

open Braces.Parser

let x, _ = parse_sexpr (fun () -> Error `empty) (explode "(lambda a{a[a[#t)")

let _ =
  print_endline
    (match x with
    | `normal x ->
        prerr_endline (Braces.Sexpr.sexpr_to_string x);
        Braces.Eval.eval Braces.Eval.Env.empty x |> Braces.Eval.sexpr_to_string
    | `close _ -> "close"
    | `empty -> "empty")
