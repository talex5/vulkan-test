module C = Common
module N = C.StringMap
module T = Refined_types

type metadata =
  { name:string;
    number:int;
    type':string option;
    version : int }

type info =
 | Third_party of { offset:int; upward:bool; extension_number: int option }
 | Core of int

type constr =
  | Value of { info: info }
  | Bit of { pos:int }

type enum =
  { extend:string; name:string; constr:constr; xml:Xml.node }

type 'a data =
  {
    metadata: 'a;
    types: string list;
    commands: string list;
    enums: enum list;
  }


type t = metadata data
type versioned =
  | Active of t
  | Promoted_to of string N.t


let only_active exts = C.List.filter_map
    (function Active x -> Some x | Promoted_to _ -> None) exts


type feature_set = string data

let pp_exttype ppf = function
  | None -> Fmt.pf ppf "support=disabled"
  | Some x -> Fmt.pf ppf "type=\"%s\"" x

let pp_metadata ppf (m:metadata) =
  Fmt.pf ppf
    "@[<hov>{name=%s;@ number=%d;@ %a;@ version=%d}@]"
    m.name m.number pp_exttype m.type' m.version


let pp_enum ppf (e:enum)=
  let pp_info ppf = function
  | Third_party ext ->
    Fmt.pf ppf "Third_party {offset=%d;@ upward=%b}"
      ext.offset ext.upward
  | Core n ->
    Fmt.pf ppf "Core %d" n in
  let pp_constr ppf = function
    | Value { info } -> pp_info ppf info
    | Bit { pos } -> Fmt.pf ppf "pos=%d" pos in
  Fmt.pf ppf
    "@[<hov>{name=%s;@ extend=%s;@ info=%a}@]"
      e.name e.extend pp_constr e.constr

let pp_active ppf (t:t) =
  Fmt.pf ppf
    "@[<v 2>{metadata=%a;@;types=[%a];@;commands=[%a];@;enums=[%a];@ }@]"
    pp_metadata t.metadata
    Fmt.(list string) t.types
    Fmt.(list string) t.commands
    (Fmt.list pp_enum) t.enums

let pp ppf = function
  | Active t ->
    pp_active ppf t
  | Promoted_to aliases ->
    Fmt.pf ppf "@[<v>Promoted extension.@,aliases:@[%a@]@]"
      Fmt.(list @@ pair string string) (N.bindings aliases)

module Extend = struct

  module Bound = struct
    type t = {inf:int;sup:int}
    let all = { inf = max_int; sup = min_int }
    let add b x = { sup = max x b.sup; inf  = min x b.inf }

    let extrema =
      List.fold_left add all
  end

  module IntMap = Map.Make(Int)

  type enum_or_bit =
    | Values of (Bound.t * (string * T.pos) IntMap.t)
    | Bitfield of ((string * int) list * (string * int) list)

  let decorate_enum constrs =
    let add' (b,emap) = function
      | _, T.Abs n as constr ->
        Bound.add b n,
        IntMap.add n constr emap
      | _ -> b, emap in
    Values (List.fold_left add' (Bound.all,IntMap.empty) constrs)


  let find m0 x m =
    try N.find x m with
    | Not_found ->
      match N.find x m0 with
      | Entity.Type Bitfields x -> Bitfield (x.fields, x.values)
      | Entity.Type Enum constrs -> decorate_enum constrs
      | Entity.Type x -> Fmt.failwith "Expected parent enum or bitfield, but found: %a" Refined_types.Ty.pp_def x
      | _ -> raise Not_found


  let enum extension_number m0 =
    let find = find m0 in
    let add m (x:enum) =
      try
        let key = x.extend in
        match find key m, x.constr with
        | Bitfield (fields, vals), Bit { pos } ->
          let l = (x.name, pos) :: fields, vals in
          N.add key (Bitfield l) m
        | Bitfield (fields, vals), Value { info = Core v } ->
          let l = fields, (x.name, v) :: vals in
          N.add key (Bitfield l) m
        | Values (b, emap), Value { info } ->
          let abs =
            match info with
            | Core n -> n
            | Third_party ext ->
              let extension_number =
                C.Option.merge_exn  ext.extension_number extension_number in
              let pos = (1000000 + extension_number - 1) * 1000 + ext.offset in
              if ext.upward then +pos else -pos in
          let elt = Bound.add b abs, IntMap.add abs (x.name, T.Abs abs) emap in
          N.add key (Values elt) m
        | _ ->
          failwith "Unexpected extension combination"
        with Failure m ->
          let bt = Printexc.get_raw_backtrace () in
          let m = Fmt.str "@[<hv>%s@; at %a@]" m Xml.pp_node_loc x.xml in
          Printexc.raise_with_backtrace (Failure m) bt
    in
    List.fold_left add N.empty

  let extend number m ext =
    let ext_num = number ext in
    let enums = enum ext_num m ext.enums in
    let list emap = List.map snd (IntMap.bindings emap) in
    let rebuild_enum key = function
      | Values (_,emap) ->
        N.add key (Entity.Type(T.Ty.Enum (list emap)))
      | Bitfield (fields,values) ->
        N.add key (Entity.Type(T.Ty.Bitfields { fields; values })) in
    m
    |> N.fold rebuild_enum enums

  let all_exts m exts =
    let number x = Some x.metadata.number in
    let exts = only_active exts in
    let exts =
      List.sort (fun x y -> compare (number x) (number y)) exts in
    List.fold_left (extend number) m exts

  let update m update =
    let number _x = None in
    List.fold_left (extend number) m update

  let all m upd exts =
    all_exts (update m upd) exts

end
