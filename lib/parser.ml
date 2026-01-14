open Sexpr

type error = Empty | Bad

type state = {
  braces : char list;
  current : string;
  result : (sexpr, error) result;
}

let init_state = { braces = []; current = ""; result = Error Empty }

let parse_sexpr (string : string) : (sexpr, error) result =
  (* maybe just go back to recursive descent *)
  (String.fold_left
     (fun state char ->
       match state.result with
       | Ok _ -> (
           match char with
           | '{' | '[' | '(' ->
               let state = { state with braces = char :: state.braces } in
               failwith ""
           | '}' | ']' | ')' -> failwith ""
           | ' ' | '\t' | '\n' -> state
           | _ -> failwith "")
       | Error Empty -> failwith ""
       | Error _ -> state)
     init_state string)
    .result
