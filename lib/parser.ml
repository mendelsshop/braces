open Sexpr

let split p l =
  let rec aux f = function
    | x :: l when p x -> aux (fun xs -> f (x :: xs)) l
    | rest -> (f [], rest)
  in
  aux Fun.id l

module StringMap = Map.Make (String)

let keywords = StringMap.of_list [ ("#t", Boolean true); ("#f", Boolean false) ]

(*
Source - https://stackoverflow.com/a
Posted by Maëlan
Retrieved 2026-01-18, License - CC BY-SA 4.0
*)

let explode (s : string) : Uchar.t list =
  let n = String.length s in
  let rec aux i =
    if i >= n then []
    else
      let d = String.get_utf_8_uchar s i in
      let k = Uchar.utf_decode_length d in
      let u = Uchar.utf_decode_uchar d in
      (* ^ if d is an invalid utf8 sequence, then k = 1
       *   and u = the replacement character (U+FFFD) *)
      u :: aux (i + k)
  in
  aux 0

(* given the closing brace get the opening brace *)
(* for a few (4) there is character between the open and close brace in the unicode/ascii spec *)
let close_open close =
  close
  - if close = 93 || close = 125 || close = 65341 || close = 65373 then 2 else 1

let rec parse_sexpr =
 fun empty_k string ->
  match string with
  | x :: string -> (
      match Uchar.to_int x with
      (* ('\t' | ' ' | '\n') *)
      | 9 | 32 | 10 ->
          parse_sexpr empty_k string
          (* '(' | '[' | '{' *)
          (* TODO: some cases of the unicode braces are seperated by another charachter like ( [ *)
      | ( 40 | 91 | 123 | 3898 | 3900 | 8261 | 8317 | 8333 | 8968 | 8970 | 10088
        | 10090 | 10092 | 10094 | 10096 | 10098 | 10100 | 10181 | 10214 | 10216
        | 10218 | 10220 | 10222 | 10627 | 10629 | 10631 | 10633 | 10635 | 10637
        | 10639 | 10641 | 10643 | 10645 | 10647 | 10712 | 10714 | 11810 | 11812
        | 11814 | 11816 | 11861 | 11863 | 11865 | 11867 | 12296 | 12298 | 12300
        | 12302 | 12304 | 12308 | 12310 | 12312 | 12314 | 65113 | 65115 | 65117
        | 65288 | 65339 | 65371 | 65375 | 65378 ) as char ->
          let rec list f string =
            let res, string = parse_sexpr empty_k string in
            match res with
            | `close (`char close) when char = close_open close ->
                (`normal (List (f [])), string)
            | `close (`expr (expr, close)) when char = close_open close ->
                (`normal (List (f [ expr ])), string)
            | `close (`expr (expr, close)) ->
                (`close (`expr (List (f [ expr ]), close)), string)
            | `close (`char close) ->
                (`close (`expr (List (f []), close)), string)
            | `normal sexpr -> list (fun x -> f (sexpr :: x)) string
            | e -> (e, string)
          in
          list Fun.id string
      (* ')' | ']' | '}' *)
      | ( 41 | 93 | 125 | 3899 | 3901 | 8262 | 8318 | 8334 | 8969 | 8971 | 10089
        | 10091 | 10093 | 10095 | 10097 | 10099 | 10101 | 10182 | 10215 | 10217
        | 10219 | 10221 | 10223 | 10628 | 10630 | 10632 | 10634 | 10636 | 10638
        | 10640 | 10642 | 10644 | 10646 | 10648 | 10713 | 10715 | 11811 | 11813
        | 11815 | 11817 | 11862 | 11864 | 11866 | 11868 | 12297 | 12299 | 12301
        | 12303 | 12305 | 12309 | 12311 | 12313 | 12315 | 65114 | 65116 | 65118
        | 65289 | 65341 | 65373 | 65376 | 65379 ) as char ->
          (`close (`char char), string)
      | _ ->
          let buffer = Buffer.create 0 in
          let symbol, string =
            split
              (fun x ->
                (*  [ '['; '{'; '('; ']'; '}'; ')'; '\t'; ' '; '\n' ]  *)
                List.mem (Uchar.to_int x)
                  [ 9; 32; 10; 40; 91; 41; 93; 125; 123 ]
                |> not)
              string
          in
          x :: symbol |> List.iter (Buffer.add_utf_8_uchar buffer);
          let var_name = Buffer.to_bytes buffer |> Bytes.to_string in
          ( `normal
              (Int64.of_string_opt var_name
              |> Option.map Int64.to_int
              |> Option.fold
                   ~none:
                     (StringMap.find_opt var_name keywords
                     |> Option.value ~default:(Symbol var_name))
                   ~some:(fun n -> Number n)),
            string )
          (* | Some i -> (`normal (Number i), string) *)
          (* | None -> (`normal (Symbol var_name), string))) *))
  | [] -> (
      let string = empty_k () in
      match string with
      | Ok string -> parse_sexpr empty_k string
      | Error e ->
          let var_name = e in
          (var_name, []))
