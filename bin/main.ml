let () = print_endline "Hello, World!"

open Braces.Parser

let x, _ = parse_sexpr (fun () -> Error `empty) (explode "(if #t 7 8)")

let _ =
  let braces =
    [ '('; 'a'; '['; 'a'; '{'; 'b'; ' '; 'c'; ']'; ')'; ']' ]
    |> List.map Uchar.of_char
  in
  let brackets = Braces.Unicode_parser.scan_brackets braces in
  let braces' = Braces.Unicode_parser.to_braced brackets braces in
  let sexpr, _left = Braces.Unicode_parser.parse braces' in
  print_endline
    ((Braces.Unicode_parser.IntMap.to_list brackets
     |> List.map (fun (start, end_) ->
         "(" ^ string_of_int start ^ ", " ^ string_of_int end_ ^ ")")
     |> String.concat "")
    ^ Braces.Sexpr.sexpr_to_string sexpr
      (* ^ (Braces.Eval.eval Braces.Eval.Env.empty sexpr *)
      (*   |> Braces.Eval.sexpr_to_string) *))

let _ =
  print_endline
    (match x with
    | `normal x ->
        prerr_endline (Braces.Sexpr.sexpr_to_string x);
        Braces.Eval.eval Braces.Eval.Env.empty x |> Braces.Eval.sexpr_to_string
    | `close _ -> "close"
    | `empty -> "empty")
