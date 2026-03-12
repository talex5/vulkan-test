(* Data shared with the shader code. *)

module Vkt = Vk.Types
module A = Vulkan.A
module Vec3 = Vulkan.Vec3
module Matrix4x4 = Vulkan.Matrix4x4

let float4x4 = Ctypes.(array 16 float)

let land_view_size = (80, 60)

type ship = {
  mutable pos : Vec3.t;
  mutable vel : Vec3.t;
  mutable pitch : float;
  mutable yaw : float;
}

(* In world coordinates, (x,y) is the position on the map and z is height.
   The x axis points right (East) and y points North, away from the viewer
   (i.e. this is a right-handled system). *)

module C = struct
  let int_uint32_t =
    Ctypes.(view uint32_t)
      ~read:Unsigned.UInt32.to_int
      ~write:Unsigned.UInt32.of_int

  type mark
  type t = mark Ctypes.structure
  let ctype : t Ctypes.typ = Ctypes.structure "uniform_buffer_object"

  let project_world = Ctypes.field ctype "project_world" float4x4
  (* Takes world coordinates, converts to view space, and then projects them in 3D.
     model (x, y) are points on the map, where (0, 0) is the SW corner.
     model z is height.
  
     After projection, x and y are the screen position (x points right
     and y points down), and z is the depth, pointing away from us
     (a right-handed system). *)

  let ship_rot = Ctypes.field ctype "ship_rot" float4x4
  let ship_pos = Ctypes.field ctype "ship_pos" Vec3.view
  let _ = Ctypes.field ctype "padding" Ctypes.float

  let camera_pos = Ctypes.field ctype "camera_pos" Vec3.view
  let _ = Ctypes.field ctype "padding" Ctypes.float

  (* The location in world-space of the NW corner of the landscape display *)
  let land_visible_nw = Ctypes.field ctype "land_visible_nw" Ctypes.(array 2 float)
  let land_view_size = Ctypes.field ctype "land_view_size" Ctypes.(array 2 int_uint32_t)

  let map_size = Ctypes.field ctype "map_size" Ctypes.(array 2 int_uint32_t)

  let () = Ctypes.seal ctype
  let size = Ctypes.sizeof ctype
end

type t = {
  info : Vkt.Descriptor_buffer_info.t;
  mapped : C.t;
}

let set_pair c p (x, y) =
  let p = Ctypes.getf c p in
  A.set p 0 x;
  A.set p 1 y

let set_constants c ~land_view_size ~map_size =
  set_pair c C.land_view_size land_view_size;
  set_pair c C.map_size map_size

let set_camera ~project_world ~camera_pos ~land_visible_nw t =
  let c = t.mapped in
  let set k v = Ctypes.setf c k v in
  let set_matrix field v = Vulkan.Matrix4x4.write v (Ctypes.getf c field) in
  set_matrix C.project_world project_world;
  set_pair c C.land_visible_nw land_visible_nw;
  set C.camera_pos camera_pos

let set_ship ~ship_rot ~ship_pos t =
  let c = t.mapped in
  let set k v = Ctypes.setf c k v in
  let set_matrix field v = Vulkan.Matrix4x4.write v (Ctypes.getf c field) in
  set_matrix C.ship_rot ship_rot;
  set C.ship_pos ship_pos

let create ~sw ~device =
  let buffer = Vulkan.Buffer.create ~sw device C.size
      ~usage:Vkt.Buffer_usage_flags.uniform_buffer
      ~properties:Vkt.Memory_property_flags.(host_visible + host_coherent)
  in
  let mapped = A.cast_one C.ctype (Vulkan.Buffer.map ~sw buffer) in
  let range = Vkt.Device_size.of_int C.size in
  let info = Vkt.Descriptor_buffer_info.make ~buffer:buffer.buffer ~offset:Vkt.Device_size.zero ~range () in
  set_constants mapped ~land_view_size ~map_size:Map.size;
  { info; mapped }
