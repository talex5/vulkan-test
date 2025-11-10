module Code = struct
  type t = int32

  let rev_if = function
    | true -> List.rev
    | false -> Fun.id

  let to_string t =
    List.init 4 (fun i -> Char.chr @@ Int32.to_int @@ Int32.logand 0xffl @@ Int32.shift_right_logical t (i * 8))
    |> rev_if Sys.big_endian
    |> List.to_seq
    |> String.of_seq

  let of_string s =
    String.to_seq s
    |> List.of_seq
    |> rev_if @@ not Sys.big_endian
    |> List.fold_left (fun acc i -> Int32.logor (Int32.of_int @@ Char.code i) @@ Int32.shift_left acc 8) 0l

  let xr24 = of_string "XR24"
end

module Mod = struct
  type t = int64
  let reserved = 0xffffffffffffffL
end

type t = {
  code : Code.t;
  modifier : Mod.t;
}

let pp f { code; modifier } =
  Fmt.pf f "%4s : %Lx" (Code.to_string code) modifier

let v code ~modifier = { code; modifier }
