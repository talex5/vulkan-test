(* Data shared with the shader code. *)

module Vkt = Vk.Types
module A = Vulkan.A
module Vec3 = Vulkan.Vec3
module Matrix4x4 = Vulkan.Matrix4x4

let float4x4 = Ctypes.(array 16 float)

type ship = {
  mutable pitch : float;
  mutable yaw : float;
}

module C = struct
  type mark
  type t = mark Ctypes.structure
  let ctype : t Ctypes.typ = Ctypes.structure "uniform_buffer_object"

  let project_world = Ctypes.field ctype "project_world" float4x4
  let ship_rot = Ctypes.field ctype "ship_rot" float4x4
  let camera_pos = Ctypes.field ctype "camera_pos" Vec3.view

  let () = Ctypes.seal ctype
  let size = Ctypes.sizeof ctype
end

type t = {
  info : Vkt.Descriptor_buffer_info.t;
  mapped : C.t;
}

let set_camera ~project_world ~camera_pos t =
  let c = t.mapped in
  let set k v = Ctypes.setf c k v in
  let set_matrix field v = Vulkan.Matrix4x4.write v (Ctypes.getf c field) in
  set_matrix C.project_world project_world;
  set C.camera_pos camera_pos

let set_ship ~ship_rot t =
  let c = t.mapped in
  let set_matrix field v = Vulkan.Matrix4x4.write v (Ctypes.getf c field) in
  set_matrix C.ship_rot ship_rot

let create ~sw ~device =
  let buffer = Vulkan.Buffer.create ~sw device C.size
      ~usage:Vkt.Buffer_usage_flags.uniform_buffer
      ~properties:Vkt.Memory_property_flags.(host_visible + host_coherent)
  in
  let mapped = A.cast_one C.ctype (Vulkan.Buffer.map ~sw buffer) in
  let range = Vkt.Device_size.of_int C.size in
  let info = Vkt.Descriptor_buffer_info.make ~buffer:buffer.buffer ~offset:Vkt.Device_size.zero ~range () in
  { info; mapped }
