module Vkt = Vk.Types
module A = Vulkan.A

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

  let tau = Float.pi *. 2.
  let z_near = 1.
  let z_far = 10.
  let fov_y = (1. /. 12.) *. tau
  let scale_y = 1. /. tan (fov_y /. 2.)

  let set t ~aspect frame_number =
    let scale_x = scale_y *. aspect in
    let model = Ctypes.getf t model in
    let th = 0.6 +. 0.7 *. sin (float frame_number /. 200.) in
    set_matrix model [
      [-.sin th ; cos th ; 0.0 ; 0. ];
      [0.       ; 0.0    ; -1.0; 0.4];
      [cos th   ; sin th ; 0.0 ; -3.];
      [0.       ; 0.     ; 0.  ; 1. ];
    ];
    let proj = Ctypes.getf t proj in
    let scale_z = (z_far +. z_near) /. (z_near -. z_far) in
    let bias_z = (2. *. z_far *. z_near) /. (z_near -. z_far) in
    (* See gluPerspective *)
    set_matrix proj [
      [scale_x ; 0.      ; 0.      ; 0.    ];	(* Normalise x for FOV *)
      [0.      ; scale_y ; 0.      ; 0.    ];	(* Normalise y for FOV *)
      [0.      ; 0.      ; scale_z ; bias_z];	(* Normalise z (for clipping) *)
      [0.      ; 0.      ; -1.     ; 0.    ];	(* Make far-away things smaller *)
    ]
end

type t = {
  info : Vkt.Descriptor_buffer_info.t;
  mapped : C.t;
}

let create ~sw ~device =
  let buffer = Vulkan.Buffer.create ~sw device C.size
      ~usage:Vkt.Buffer_usage_flags.uniform_buffer
      ~properties:Vkt.Memory_property_flags.(host_visible + host_coherent)
  in
  let mapped = A.cast_one C.ctype (Vulkan.Buffer.map ~sw buffer) in
  let range = Vkt.Device_size.of_int C.size in
  let info = Vkt.Descriptor_buffer_info.make ~buffer:buffer.buffer ~offset:Vkt.Device_size.zero ~range () in
  { info; mapped }

let set t ~geometry:(width, height) frame_number =
  let aspect = float height /. float width in
  C.set t.mapped ~aspect frame_number
