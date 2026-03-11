(* Data shared with the shader code. *)

module Vkt = Vk.Types
module A = Vulkan.A
module Vec3 = Vulkan.Vec3
module Matrix4x4 = Vulkan.Matrix4x4

let float4x4 = Ctypes.(array 16 float)

module C = struct
  type mark
  type t = mark Ctypes.structure
  let ctype : t Ctypes.typ = Ctypes.structure "uniform_buffer_object"
  let model = Ctypes.field ctype "model" float4x4
  let project_world = Ctypes.field ctype "project_world" float4x4
  let () = Ctypes.seal ctype
  let size = Ctypes.sizeof ctype
end

type t = {
  info : Vkt.Descriptor_buffer_info.t;
  mapped : C.t;
}

let set ~model ~project_world t =
  let c = t.mapped in
  let set_matrix field v = Vulkan.Matrix4x4.write v (Ctypes.getf c field) in
  set_matrix C.model model;
  set_matrix C.project_world project_world

let create ~sw ~device =
  let buffer = Vulkan.Buffer.create ~sw device C.size
      ~usage:Vkt.Buffer_usage_flags.uniform_buffer
      ~properties:Vkt.Memory_property_flags.(host_visible + host_coherent)
  in
  let mapped = A.cast_one C.ctype (Vulkan.Buffer.map ~sw buffer) in
  let range = Vkt.Device_size.of_int C.size in
  let info = Vkt.Descriptor_buffer_info.make ~buffer:buffer.buffer ~offset:Vkt.Device_size.zero ~range () in
  { info; mapped }
