open Sexpr

let split p l =
  let rec aux f = function
    | x :: l when p x -> aux (fun xs -> f (x :: xs)) l
    | rest -> (f [], rest)
  in
  aux Fun.id l

(*
Source - https://stackoverflow.com/a
Posted by Maëlan
Retrieved 2026-01-18, License - CC BY-SA 4.0
*)

let uchar_seq_of_utf8 (s : string) : Uchar.t Seq.t =
  let n = String.length s in
  let rec aux i () =
    if i >= n then Seq.Nil
    else
      let d = String.get_utf_8_uchar s i in
      let k = Uchar.utf_decode_length d in
      let u = Uchar.utf_decode_uchar d in
      (* ^ if d is an invalid utf8 sequence, then k = 1
       *   and u = the replacement character (U+FFFD) *)
      Seq.Cons (u, aux (i + k))
  in
  aux 0

let rec parse_sexpr =
 fun empty_k string ->
  match string with
  (* ('\t' | ' ' | '\n') *)
  | (9 | 32 | 10) :: string -> parse_sexpr empty_k string
  (* '(' | '[' | '{' *)
  | (( 40 | 91
     | 123
       (*    ( "(" | "༺" | "༼" | "⁅" | "⁽" | "₍" | "⌈" | "⌊" | "❨" | "❪" | "❬" | "❮" *)
       (* | "❰" | "❲" | "❴" | "⟅" | "⟦" | "⟨" | "⟪" | "⟬" | "⟮" | "⦃" | "⦅" | "⦇" *)
       (* | "⦉" | "⦋" | "⦍" | "⦏" | "⦑" | "⦓" | "⦕" | "⦗" | "⧘" | "⧚" | "⸢" | "⸤" *)
       (* | "⸦" | "⸨" | "\u{2e55}" | "\u{2e57}" | "\u{2e59}" | "\u{2e5b}" | "〈" | "《" *)
       (* | "「" | "『" | "【" | "〔" | "〖" | "〘" | "〚" | "﹙" | "﹛" | "﹝" | "（" | "［" *)
       (* | "｛" | "｟" | "｢" | "{" | "[" )  *) ) as char)
    :: string ->
      let rec list f string =
        let res, string = parse_sexpr empty_k string in
        match res with
        | `close (`char close)
          when char = close - if close = 93 || close = 125 then 2 else 1 ->
            (`normal (List (f [])), string)
        | `close (`expr (expr, close))
          when char = close - if close = 93 || close = 125 then 2 else 1 ->
            (`normal (List (f [ expr ])), string)
        | `close (`expr (expr, close)) ->
            (`close (`expr (List (f [ expr ]), close)), string)
        | `close (`char close) -> (`close (`expr (List (f []), close)), string)
        | `normal sexpr -> list (fun x -> f (sexpr :: x)) string
        | e -> (e, string)
      in
      list Fun.id string
  (* ')' | ']' | '}' *)
  | ((41 | 93 | 125) as char) :: string -> (`close (`char char), string)
  | char :: string ->
      let buffer = Buffer.create 0 in
      let symbol, string =
        split
          (fun x ->
            (*  [ '['; '{'; '('; ']'; '}'; ')'; '\t'; ' '; '\n' ]  *)
            List.mem x [ 9; 32; 10; 40; 91; 41; 93; 125; 123 ] |> not)
          string
      in
      char :: symbol |> List.map Uchar.of_int
      |> List.iter (Buffer.add_utf_8_uchar buffer);
      (`normal (Symbol (Buffer.to_bytes buffer |> Bytes.to_string)), string)
  | [] -> (
      let string = empty_k () in
      match string with
      | Ok string -> parse_sexpr empty_k string
      | Error e ->
          let var_name = e in
          (var_name, []))
