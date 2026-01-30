(* TODO: (support ltr languages) *)
let close_open close =
  (Char.code close - if close = ']' || close = '}' then 2 else 1) |> Char.chr

(* from https://www.unicode.org/notes/tn39/#ReviewModule *)
(* Review Module for Bidi Brackets for Dummies *)
(* "[" is a bracket. *)
(* "(" is a "bracket", too. *)
(* "[" is opening, and pairs with "]", which is closing. *)
(* "{..}" contains a bracket pair. "}..{" does not. *)
(* "(..[)..]" → (..[), but neither "[" nor "]" is part of a pair. *)
(* "[(]x[)]" → [(] and [)], but neither "(" nor ")" is part of a pair. *)
(* if its not a pair we will leave as a symol, which might reinterpret the whole sexpr, so maybe scan for pairs, and the use continuations or the like to actually construct the sexpr after the braces are fullly scanned (closed) *)
let rec parse brackets = function
  | (('(' | '[' | '{') as open_c) :: string -> (
      let close, string' = parse brackets string in
      match close with
      | `close close when close_open close = open_c ->
          (`normal (Sexpr.List []), string')
      | `close close ->
          (`close_symbol (close, String.make 1 open_c, []), string')
      | `close_symbol (close, close_string, list) when close_open close = open_c
        ->
          (`normal (Sexpr.List (Sexpr.Symbol close_string :: list)), string')
      | `close_symbol (close, close_string, list) ->
          ( `close_symbol (close, String.make 1 open_c ^ close_string, list),
            string' )
      | `normal _ -> (`normal_symbol (String.make 1 open_c), string)
      | `normal_symbol symbol ->
          (`normal_symbol (String.make 1 open_c ^ symbol), string))
  | ((')' | ']' | '}') as close) :: string ->
      if List.mem (close_open close) brackets then (`close close, string)
      else failwith ""
  | (' ' | '\n' | '\t') :: string -> (
      let close, string' = parse brackets string in

      (* TODO: what to do about "  ([]" (should be parsed as Symbol ( *)
      match close with
      | `close close -> (`close close, string')
      | `close_symbol (close, symbol, list) ->
          (* TODO: this cause an empty symbol in "(  a)"  *)
          (`close_symbol (close, "", Sexpr.Symbol symbol :: list), string')
      | `normal _ | `normal_symbol _ -> failwith "")
  | x :: string -> failwith ""
  | [] -> failwith ""
