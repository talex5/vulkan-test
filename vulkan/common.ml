include Eio.Std
module Vkt = Vk.Types
module Vkc = Vk.Core
module A = struct
  include Ctypes.CArray

  let sizeof t = Ctypes.sizeof (element_type t) * length t

  (* Ctypes seems to be missing a way to cast arrays. *)
  let cast new_elt t =
    let old_elt = element_type t in
    let p = start t in
    let len_bytes = length t * Ctypes.sizeof old_elt in
    let p = Ctypes.(coerce (ptr old_elt) (ptr new_elt) p) in
    let t' = from_ptr p (len_bytes / Ctypes.sizeof new_elt) in
    Vk__helpers.keep_alive t t';
    t'

  let cast_one new_elt t =
    let t = cast new_elt t in
    assert (length t = 1);
    get t 0

  let split (t : char t) len =
    assert (len <= length t);
    let a = from_ptr (start t) len in
    let b = from_ptr Ctypes.(start t +@ len) (length t - len) in
    Vk__helpers.keep_alive t a;
    Vk__helpers.keep_alive t b;
    a, b
end

let ( <?> ) x s = match x with
  | Ok (r, x) -> Log.debug (fun f -> f "%a: %s" Vkt.Result.raw_pp r s); x
  | Error k -> Fmt.failwith "Error %a: %s" Vkt.Result.raw_pp k s

(* For the [pNext] extension fields, we need to convert to void pointers explicitly. *)
let ext = Ctypes.to_voidp

(* [A.of_list Ctypes.string] doesn't work because it doesn't register the
   C strings with the GC. This is a work-around for that. *)
let string_array xs =
  let xs = List.map Ctypes.(coerce string (ptr char)) xs in     (* Make the C strings *)
  let arr = A.of_list Ctypes.(ptr char) xs in                   (* Make the array *)
  let p = Ctypes.to_voidp arr.astart |> Ctypes.(from_voidp string) in
  let arr = A.from_ptr p arr.alength in                         (* Cast to string array *)
  Vk__helpers.keep_alive xs arr;
  arr

(* Float arrays don't have the above problem, but add a helper so we don't
   have to remember when [A.of_list] is safe and when it isn't. *)
let float_array = A.of_list Ctypes.float
