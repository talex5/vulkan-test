module Vkt = Vk.Types
module A = Vulkan.A
module Vec3 = Vulkan.Vec3
module Matrix4x4 = Vulkan.Matrix4x4

let float4x4 = Ctypes.(array 16 float)

let set_matrix t rows =
  let width = List.length (List.hd rows) in
  rows |> List.iteri (fun ri row ->
      row |> List.iteri (fun ci x ->
          A.set t (ci * width + ri) x
        )
    )

(* Data shared with the shader code. *)
module C = struct
  type mark
  type t = mark Ctypes.structure
  let ctype : t Ctypes.typ = Ctypes.structure "Uniform_buffer_object"
  let model = Ctypes.field ctype "model" float4x4
  let proj = Ctypes.field ctype "proj" float4x4
  let () = Ctypes.seal ctype
  let size = Ctypes.sizeof ctype
end

let tau = Float.pi *. 2.
let z_near = 1.
let z_far = 10.
let fov_y = (1. /. 12.) *. tau

type t = {
  info : Vkt.Descriptor_buffer_info.t;
  mapped : C.t;
}

let set t ~geometry:(width, height) frame_number =
  let aspect = float width /. float height in
  let c = t.mapped in
  let set_matrix field v = Matrix4x4.write v (Ctypes.getf c field) in
  let th = tau /. 4. +. 0.6 +. 0.7 *. sin (float frame_number /. 200.) in
  set_matrix C.model Matrix4x4.(
      translate (Vec3.v 0. 0.4 (-3.)) *
      rot_y th *
      rot_x (tau /. 4.0) *
      scale_y (-1.0)
    );
  set_matrix C.proj (Matrix4x4.(perspective_projection ~fov_y ~aspect ~z_near ~z_far))

let create ~sw ~device =
  let buffer = Vulkan.Buffer.create ~sw device C.size
      ~usage:Vkt.Buffer_usage_flags.uniform_buffer
      ~properties:Vkt.Memory_property_flags.(host_visible + host_coherent)
  in
  let mapped = A.cast_one C.ctype (Vulkan.Buffer.map ~sw buffer) in
  let range = Vkt.Device_size.of_int C.size in
  let info = Vkt.Descriptor_buffer_info.make ~buffer:buffer.buffer ~offset:Vkt.Device_size.zero ~range () in
  { info; mapped }
