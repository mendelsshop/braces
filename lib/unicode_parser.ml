(* TODO: (support ltr languages) *)

(* given the closing brace get the opening brace *)
(* for a few (4) there is character between the open and close brace in the unicode/ascii spec *)
let close_open close =
  close
  - if close = 93 || close = 125 || close = 65341 || close = 65373 then 2 else 1

module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)

let keywords =
  StringMap.of_list [ ("#t", Sexpr.Boolean true); ("#f", Sexpr.Boolean false) ]

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
let split_half p l =
  let rec aux = function
    | x :: l when p x -> Some (x, l)
    | _ :: l -> aux l
    | [] -> None
  in
  aux l

let split_map p l =
  let rec aux f = function
    | x :: l ->
        (p x
        |> Option.fold
             ~some:(fun x' () -> aux (fun xs -> f (x' :: xs)) l)
             ~none:(fun () -> (f [], x :: l)))
          ()
    | [] -> (f [], [])
  in
  aux Fun.id l

let scan_brackets =
  let rec aux starts i string =
    match string with
    | x :: list -> (
        match Uchar.to_int x with
        | ( 40 | 91 | 123 | 3898 | 3900 | 8261 | 8317 | 8333 | 8968 | 8970
          | 10088 | 10090 | 10092 | 10094 | 10096 | 10098 | 10100 | 10181
          | 10214 | 10216 | 10218 | 10220 | 10222 | 10627 | 10629 | 10631
          | 10633 | 10635 | 10637 | 10639 | 10641 | 10643 | 10645 | 10647
          | 10712 | 10714 | 11810 | 11812 | 11814 | 11816 | 11861 | 11863
          | 11865 | 11867 | 12296 | 12298 | 12300 | 12302 | 12304 | 12308
          | 12310 | 12312 | 12314 | 65113 | 65115 | 65117 | 65288 | 65339
          | 65371 | 65375 | 65378 ) as open_c ->
            aux ((open_c, i) :: starts) (i + 1) list
        | ( 41 | 93 | 125 | 3899 | 3901 | 8262 | 8318 | 8334 | 8969 | 8971
          | 10089 | 10091 | 10093 | 10095 | 10097 | 10099 | 10101 | 10182
          | 10215 | 10217 | 10219 | 10221 | 10223 | 10628 | 10630 | 10632
          | 10634 | 10636 | 10638 | 10640 | 10642 | 10644 | 10646 | 10648
          | 10713 | 10715 | 11811 | 11813 | 11815 | 11817 | 11862 | 11864
          | 11866 | 11868 | 12297 | 12299 | 12301 | 12303 | 12305 | 12309
          | 12311 | 12313 | 12315 | 65114 | 65116 | 65118 | 65289 | 65341
          | 65373 | 65376 | 65379 ) as close ->
            let open_c = close_open close in
            let open_p = split_half (Fun.compose (( = ) open_c) fst) starts in
            let starts =
              open_p |> Option.map snd |> Option.value ~default:starts
            in
            let result = aux starts (i + 1) list in
            open_p |> Option.map fst
            |> Option.fold ~none:result ~some:(fun (_, i_start) ->
                IntMap.add i_start i result)
        | _ -> aux starts (i + 1) list)
    | [] -> IntMap.empty
  in
  aux [] 0

let hd = function [] -> None | a :: _ -> Some a

let to_braced brackets string =
  let rec aux i closers brackets = function
    | x :: string ->
        let opener = IntMap.find_opt i brackets in
        let current, closers =
          Option.fold opener
            ~some:(fun current -> (`opener x, current :: closers))
            ~none:
              (match closers with
              | i' :: closers when i = i' -> (`closer x, closers)
              | _
                when Uchar.to_int x = 9
                     || Uchar.to_int x = 32
                     || Uchar.to_int x = 10 ->
                  (`whitespace x, closers)
              | _ -> (`regular x, closers))
        in
        current :: aux (i + 1) closers brackets string
    | [] -> []
  in
  aux 0 [] brackets string

let rec eat_whitespace = function
  | `whitespace _ :: string -> eat_whitespace string
  | s -> s

let parse list =
  let rec aux k =
    let apply_k k (v, string) =
      match k with
      | `single -> (v, string)
      | `multi k -> aux (`multi (fun list -> k (v :: list))) string
    in
    function
    | `whitespace _ :: string -> aux k string
    | `regular x :: string ->
        let current, rest =
          split_map (function `regular x -> Some x | _ -> None) string
        in
        let buffer = Buffer.create 0 in
        x :: current |> List.iter (Buffer.add_utf_8_uchar buffer);
        let var_name = Buffer.to_bytes buffer |> Bytes.to_string in
        apply_k k
          ( Int64.of_string_opt var_name
            |> Option.map Int64.to_int
            |> Option.fold
                 ~none:
                   (StringMap.find_opt var_name keywords
                   |> Option.value ~default:(Sexpr.Symbol var_name))
                 ~some:(fun n -> Number n),
            rest )
    | `closer _ :: string -> (
        match k with `single -> failwith "" | `multi k -> (k [], string))
    | `opener _ :: string ->
        apply_k k (aux (`multi (fun list -> Sexpr.List list)) string)
    | [] -> failwith ""
  in
  aux `single list
