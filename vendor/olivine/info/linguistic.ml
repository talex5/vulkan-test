module Dict = struct
  type word_end = string list
  type t =
    | End of word_end
    | Node of { word_end:word_end; forest: t array }

  let empty = End []

  let pos = function
    | '0'..'9' as c -> Some( Char.code c - Char.code '0' )
    | 'A'..'Z' as c -> Some( 10 + Char.code c - Char.code 'A' )
    | 'a'..'z' as c -> Some( 36 + Char.code c - Char.code 'a' )
    | _ -> None

  let make_node () = Array.make 62 empty

  let rec add_sub word read_as start stop tree =
    if start >= stop then
      match tree with
      | End _  -> End read_as
      | Node r -> Node { r with word_end = read_as }
    else
      match pos word.[start] with
      | None -> tree
      | Some c ->
        match tree with
        | End word_end ->
          let forest = make_node () in
          forest.(c) <- add_sub word read_as (start+1) stop empty;
          Node { word_end; forest }
        | Node a ->
          let t' = add_sub word read_as (start+1) stop a.forest.(c) in
          a.forest.(c) <- t'; Node a

  let add word ?(custom=[word]) tree = add_sub word custom 0 (String.length word) tree


  let read_char char = function
    | End _ -> None
    | Node a ->
      match pos char with
      | None -> None
      | Some p ->
        match a.forest.(p) with
        | End [] -> None
        | x -> Some x

  let word_end = function
    | End l | Node {word_end = l; _ } -> l

  let terminal = function
    | End l -> l
    | Node _ -> []

  let rec read_subword w current stop t =
    if current = stop then
      word_end t, stop
    else match terminal t with
      | _ :: _ as l -> l, current
      | [] ->
        match read_char w.[current] t with
        | None -> [], stop
        | Some t -> read_subword w (current+1) stop t

  (* Look up [w] in [t], stopping at either the end of [w] or
     when the dictionary has no more options.
     Returns the value at the point where it stopped, and the location. *)
  let read_word_all w start t =
    read_subword w start (String.length w) t

end

let is_num = function
  | '0' .. '9' -> true
  | _ -> false

let split_sticky_camel_case dict s =
  let mx = String.length s in
  let sub first after = String.sub s first (after-first) in
  let rec lower acc start n =
    let c = s.[n] in
    if Char.lowercase_ascii c <> c then
      capital lower (sub start n :: acc) n
    else if is_num c then
        capital numeric (sub start n :: acc) n
    else if n + 1 < mx then
      lower acc start (n+1)
    else
      sub start (n+1) :: acc
  and capital k acc start =
    if start = mx then acc else
    let word_stop, n =
    match Dict.read_word_all s start dict with
    |  _ :: _ as l, stop -> l, stop
    | [], _ -> [], start + 1 in
    let stop = n = mx in
    match word_stop with
      | _ :: _ as l -> capital lower ( List.rev_append l  acc ) n
      | [] when stop -> sub start n :: acc
      | [] ->  k acc start n
  and numeric acc start n =
    let c = s.[n] in
    if not (is_num c) then
      capital lower (sub start n::acc) n
    else if n + 1 < mx then
      numeric acc start (n+1)
    else
      sub start (n+1) :: acc in
  List.rev @@ capital lower [] 0


let lower s = String.lowercase_ascii s

type role = Prefix | Main | Postfix | Extension

type name = { prefix: string list; main: string list; postfix: string list }

let to_path n = n.prefix @ n.main @ List.rev n.postfix

let rec clean = function
  | "" :: q -> clean q
  | p -> p


module M = Common.StringMap
type dict = { words:Dict.t; roles: role M.t; context:name }

let string_prefix s s2 =
  let n = String.length s in
  String.length s2 >= n &&
  s = String.sub s2 0 n


(* Split [name] on underscores or (if no underscores) based on case.
   Also, remove any "PFN_" prefix and convert to lower-case. *)
let path dict name =
  let path =
    if string_prefix "PFN_" name then
      split_sticky_camel_case dict.words
      @@ String.sub name 4 (String.length name - 4)
    else if String.contains name '_' then
      name
      |> String.split_on_char '_'
    else
      name |> split_sticky_camel_case dict.words
  in
  path |> List.map lower |> clean

let prepath = function
  | [] -> []
  | ("p"|"pp") :: q | q -> q

(* [from_path dict path] splits [path] into prefix/main/postfix parts, according to [dict.roles].
   Also uses [prepath] for some extra clean-up first. *)
let from_path dict path =
  let empty = { prefix = []; main = []; postfix = [] } in
  let rec pre acc = function
    | [] -> acc
    | a :: q as l ->
      begin match M.find a dict.roles with
        | exception Not_found -> main acc l
        | Prefix -> pre { acc with prefix = a :: acc.prefix } q
        | Main | Postfix | Extension -> main acc l
      end
  and main acc = function
    | [] -> acc
    | a :: q as l ->
      begin match M.find a dict.roles with
        | exception Not_found -> main { acc with main = a :: acc.main } q
        | Prefix | Main -> main { acc with main = a :: acc.main } q
        | Postfix | Extension -> postfix acc l
      end
  and postfix acc = function
    | [] -> acc
    | a :: q ->
      postfix { acc with postfix = a :: acc.postfix } q in
  let r = pre empty @@ prepath path in
  {r with prefix = List.rev r.prefix; main = List.rev r.main }

let mu = { main = []; prefix = []; postfix = [] }
let simple path = { mu with main = path}
let (~:) x = simple [x]

let remove_prefix prefix name =
  let rec remove_prefix name prefix current =
    match prefix, current with
    | [] , l -> l
    | x :: q, y :: q' when x = y -> remove_prefix name q q'
    | _ :: _, _ -> name in
  remove_prefix name prefix name


let remove_context context a =
  { prefix = remove_prefix context.prefix a.prefix;
    main = remove_prefix context.main a.main;
    postfix = remove_prefix context.postfix a.postfix
  }

let make dict original =
  remove_context dict.context
  @@ from_path dict @@ path dict original

let snake ppf () = Fmt.pf ppf "_"

let escape_word s =
    begin match s.[0] with
      | '0'..'9' -> "n" ^ s
      | _ -> s
    end

let escape = function
  | ["module"] -> [ "module'" ]
  | ["type"  ] -> [ "typ" ]
  | ["object"] -> [ "object'" ]
  | ["false" ] -> [ "false'" ]
  | ["true" ] -> [ "true'" ]
  | ["inherit"] -> ["inherit'"]
  | ["function"] -> ["function'"]
  | s :: q -> escape_word s ::  q
  | p -> p

let flatten name =
  name.prefix @ name.main @ List.rev name.postfix

let full_pp ppf n =
  let pp_w ppf s =
    Fmt.string ppf s in
  let sep ppf ()  = Fmt.pf ppf "—" in
  let list = Fmt.list ~sep pp_w in
  Fmt.pf ppf "{%a|%a|%a}"
    list n.prefix list n.main list (List.rev n.postfix)


let is_extension dict = function
  | { postfix = a :: _ ; _ } when M.find_opt a dict.roles = Some Extension
    -> true
  | _ -> false

let pp_module ppf n =
  match flatten n with
  | [] -> assert false
  | [a] -> Fmt.pf ppf "%s" ( String.capitalize_ascii @@ escape_word a)
  | a :: q ->
    Fmt.pf ppf "%s_%a" (String.capitalize_ascii @@ escape_word a)
      (Fmt.list ~sep:snake Fmt.string) q

let pp_constr ppf = pp_module ppf

let pp_type ppf p =
  Fmt.list ~sep:snake Fmt.string ppf (escape @@ flatten p)

let pp_var ppf = pp_type ppf


let (//) x s = { x with postfix = s :: x.postfix }
