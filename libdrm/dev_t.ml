include PosixTypes.Dev

open struct
  external makedev : int -> int -> int64 = "caml_libdrm_makedev"
  external major : int64 -> int = "caml_libdrm_major"
  external minor : int64 -> int = "caml_libdrm_minor"
end

let of_raw x =
  assert (not Sys.big_endian);
  let aux c acc = logor (shift_left acc 8) (of_int (Char.code c)) in
  String.fold_right aux x zero

let make (major, minor) =
  of_int64 (makedev major minor)

let pp f t =
  let x = to_int64 t in
  Fmt.pf f "%d:%d" (major x) (minor x)
