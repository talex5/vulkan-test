module Vkt = Vk.Types
module A = Vulkan.A

type mark
type t = mark Ctypes.structure
let ctype : t Ctypes.typ = Ctypes.structure "Vertex"

let pos = Ctypes.field ctype "pos" Ctypes.(array 3 float)
let tex_coord = Ctypes.field ctype "tex_coord" Ctypes.(array 2 float)

let () = Ctypes.seal ctype

let make (x, y, z) (u, v) =
  let t = Ctypes.make ctype in
  Ctypes.setf t pos (A.of_list Ctypes.float [x; y; z]);
  Ctypes.setf t tex_coord (A.of_list Ctypes.float [u; v]);
  t

let attr_desc =
  let make location field format =
    let offset = Ctypes.offsetof field in
    Vkt.Vertex_input_attribute_description.make ~binding:0 ~location ~format ~offset
  in
  Vkt.Vertex_input_attribute_description.array [
    make 0 pos       R32g32b32_sfloat;
    make 1 tex_coord R32g32_sfloat;
  ]
