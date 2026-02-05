let () = print_endline "Hello, World!"

open Braces.Parser

let x, _ = parse_sexpr (fun () -> Error `empty) (explode "(if #t 7 8)")

let _ =
  let x = Braces.Unicode_parser.scan_brackets [ '('; '['; ')'; ']' ] in
  print_endline
    (Braces.Unicode_parser.StringMap.to_list x
    |> List.map (fun (s, (start, end_)) ->
        s ^ ": " ^ string_of_int start ^ ", " ^ string_of_int end_)
    |> String.concat "")

let _ =
  print_endline
    (match x with
    | `normal x ->
        prerr_endline (Braces.Sexpr.sexpr_to_string x);
        Braces.Eval.eval Braces.Eval.Env.empty x |> Braces.Eval.sexpr_to_string
    | `close _ -> "close"
    | `empty -> "empty")
