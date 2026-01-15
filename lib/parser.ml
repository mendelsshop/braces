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
  | (('[' | '{' | '(') as char) :: string ->
      let rec list f string =
        let res, string = parse_sexpr string in
        match res with
        | `close (`char close) when char = Char.chr (Char.code close - 1) ->
            (`normal (List (f [])), string)
        | `close (`expr (expr, close)) when char = Char.chr (Char.code close - 1)
          ->
            (`normal (List (f [ expr ])), string)
        | `close (`expr (expr, close)) ->
            (`close (`expr (List (f [ expr ]), close)), string)
        | `close (`char close) -> (`close (`expr (List (f []), close)), string)
        | `empty -> (`empty, string)
        | `normal sexpr -> list (fun x -> f (sexpr :: x)) string
      in
      list Fun.id string
  | ((']' | '}' | ')') as char) :: string -> (`close (`char char), string)
  | ('\t' | ' ' | '\n') :: string -> parse_sexpr string
  | char :: string ->
      let symbol, string =
        split
          (fun x ->
            List.mem x [ '['; '{'; '('; ']'; '}'; ')'; '\t'; ' '; '\n' ] |> not)
          string
      in
      (`normal (Symbol (char :: symbol |> List.to_seq |> String.of_seq)), string)
  | [] -> (`empty, [])
