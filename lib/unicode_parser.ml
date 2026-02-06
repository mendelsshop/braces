(* TODO: (support ltr languages) *)
let close_open close =
  (Char.code close - if close = ']' || close = '}' then 2 else 1) |> Char.chr

module StringMap = Map.Make (String)

(* from https://www.unicode.org/notes/tn39/#ReviewModule *)
(* Review Module for Bidi Brackets for Dummies *)
(* "[" is a bracket. *)
(* "(" is a "bracket", too. *)
(* "[" is opening, and pairs with "]", which is closing. *)
(* "{..}" contains a bracket pair. "}..{" does not. *)
(* "(..[)..]" → (..[), but neither "[" nor "]" is part of a pair. *)
(* "[(]x[)]" → [(] and [)], but neither "(" nor ")" is part of a pair. *)
(* if its not a pair we will leave as a symol, which might reinterpret the whole sexpr, so maybe scan for pairs, and the use continuations or the like to actually construct the sexpr after the braces are fullly scanned (closed) *)
(* incorrect algorithim, but idea is to find bracket pairs *)
(* and then when actually parsing when we find a something in position of bracket pair begin parsing list *)
let split p l =
  let rec aux = function
    | x :: l when p x -> Some (x, l)
    | _ :: l -> aux l
    | [] -> None
  in
  aux l

let scan_brackets =
  let rec aux starts i = function
    | (('(' | '[' | '{') as open_c) :: list ->
        aux ((String.make 1 open_c, i) :: starts) (i + 1) list
    | ((')' | ']' | '}') as close) :: list ->
        let open_c = String.make 1 (close_open close) in
        let open_p = split (Fun.compose (( = ) open_c) fst) starts in
        let starts = open_p |> Option.map snd |> Option.value ~default:starts in
        let result = aux starts (i + 1) list in
        open_p |> Option.map fst
        |> Option.fold ~none:result ~some:(fun (_, i_start) ->
            StringMap.add open_c (i_start, i) result)
    | _ :: list -> aux starts (i + 1) list
    | [] -> StringMap.empty
  in
  aux [] 0

(* let rec parse brackets = function *)
(*   | (('(' | '[' | '{') as open_c) :: string -> ( *)
(*       let close, string' = parse brackets string in *)
(*       match close with *)
(*       | `close close when close_open close = open_c -> *)
(*           (`normal (`List []), string') *)
(*       | `close close -> *)
(*           (`close_symbol (close, String.make 1 open_c, []), string') *)
(*       | `close_symbol (close, close_string, list) when close_open close = open_c *)
(*         -> *)
(*           (`normal (`List (`Symbol close_string :: list)), string') *)
(*       | `close_symbol (close, close_string, list) -> *)
(*           ( `close_symbol (close, String.make 1 open_c ^ close_string, list), *)
(*             string' ) *)
(*       | `normal _ -> (`normal_symbol (String.make 1 open_c), string) *)
(*       | `normal_symbol symbol -> *)
(*           (`normal_symbol (String.make 1 open_c ^ symbol), string)) *)
(*   | ((')' | ']' | '}') as close) :: string -> *)
(*       if List.mem (close_open close) brackets then (`close close, string) *)
(*       else failwith "" *)
(*   | (' ' | '\n' | '\t') :: string -> ( *)
(*       let close, string' = parse brackets string in *)
(**)
(*       (* TODO: what to do about "  ([]" (should be parsed as Symbol ( *) *)
(*       match close with *)
(*       | `close close -> (`close close, string') *)
(*       | `close_symbol (close, symbol, list) -> *)
(*           (* TODO: this cause an empty symbol in "(  a)"  *) *)
(*           (`close_symbol (close, "", `Symbol symbol :: list), string') *)
(*       | `normal _ | `normal_symbol _ -> failwith "") *)
(*   | x :: string -> ( *)
(*       let close, string' = parse brackets string in *)
(**)
(*       (* TODO: what to do about "  ([]" (should be parsed as Symbol ( *) *)
(*       match close with *)
(*       | `close close -> (`close close, string') *)
(*       | `close_symbol (close, symbol, list) -> *)
(*           (* TODO: this cause an empty symbol in "(  a)"  *) *)
(*           (`close_symbol (close, "", `Symbol symbol :: list), string') *)
(*       | `normal _ | `normal_symbol _ -> failwith "") *)
(*   | [] -> failwith "" *)
