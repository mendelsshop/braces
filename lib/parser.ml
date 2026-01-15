open Sexpr

type error = Empty | Bad

type state = {
  braces : char list;
  current : string;
  result : (sexpr, error) result;
}

let init_state = { braces = []; current = ""; result = Error Empty }

let split p l =
  let rec aux f = function
    | x :: l when p x -> aux (fun xs -> f (x :: xs)) l
    | rest -> (f [], rest)
  in
  aux Fun.id l

let rec parse_sexpr (string : char list) =
  match string with
  | ('[' | '{' | '(') :: string -> failwith ""
  | (']' | '}' | ')') :: string -> failwith ""
  | ('\t' | ' ' | '\n') :: string -> parse_sexpr string
  | char :: string ->
      let symbol, string =
        split
          (fun x ->
            List.mem x [ '['; '{'; '('; ']'; '}'; ')'; '\t'; ' '; '\n' ])
          string
      in
      (`Normal (Symbol (char :: symbol |> List.to_seq |> String.of_seq)), string)
  | [] -> (`empty, [])
