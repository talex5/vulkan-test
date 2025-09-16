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
module Uniform_buffer_object = struct
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

let bindings = Vulkan.Binding.[
  v 0 Uniform_buffer         Vkt.Shader_stage_flags.vertex;
  v 1 Combined_image_sampler Vkt.Shader_stage_flags.fragment;
]

type t = {
  mapped : Uniform_buffer_object.t;             (* Input data *)
  descriptor_set : Vkt.Descriptor_set.t;        (* How to use it in the pipeline *)
  pipeline_layout : Vkt.Pipeline_layout.t       (* Metadata about the input format *)
}

let sampler ~sw device =
  Vulkan.Device.create_sampler ~sw device @@
  Vkt.Sampler_create_info.make ()
    ~min_filter:Vkt.Filter.Linear
    ~mag_filter:Vkt.Filter.Linear
    ~flags:Vkt.Sampler_create_flags.empty
    ~mipmap_mode:Linear
    ~address_mode_u:Repeat
    ~address_mode_v:Repeat
    ~address_mode_w:Repeat
    ~mip_lod_bias:0.0
    ~anisotropy_enable:false
    ~max_anisotropy:1.0
    ~compare_enable:false
    ~compare_op:Always
    ~min_lod:0.0
    ~max_lod:0.0
    ~border_color:Vkt.Border_color.Int_opaque_black
    ~unnormalized_coordinates:false

let create ~sw ~device ~texture =
  let max_sets = 2 in
  let layout = Vulkan.Descriptor_set.make_layout ~sw device bindings in
  let pipeline_layout =
    Vulkan.Device.create_pipeline_layout ~sw device @@
    Vkt.Pipeline_layout_create_info.make ()
      ~set_layouts:(Vkt.Descriptor_set_layout.array [layout])
  in
  let pool = Vulkan.Descriptor_set.create_pool ~sw device ~max_sets [
      Uniform_buffer, max_sets;
      Combined_image_sampler, max_sets;
    ] in
  let descriptor_sets = Vulkan.Descriptor_set.allocate pool layout max_sets in
  let input i =
    let descriptor_set = A.get descriptor_sets i in
    let ubo = Vulkan.Buffer.create ~sw device Uniform_buffer_object.size
        ~usage:Vkt.Buffer_usage_flags.uniform_buffer
        ~properties:Vkt.Memory_property_flags.(host_visible + host_coherent)
    in
    let mapped = A.cast_one Uniform_buffer_object.ctype (Vulkan.Buffer.map ~sw ubo) in
    let range = Vkt.Device_size.of_int Uniform_buffer_object.size in
    let ubo = Vkt.Descriptor_buffer_info.make ~buffer:ubo.buffer ~offset:Vkt.Device_size.zero ~range () in
    let texture = Vkt.Descriptor_image_info.make ~image_view:texture
        ~sampler:(sampler ~sw device)
        ~image_layout:Shader_read_only_optimal
    in
    Vulkan.Descriptor_set.update device ~writes:[
      Vulkan.Descriptor_set.write descriptor_set Uniform_buffer         [ubo]     ~dst_binding:0 ~dst_array_element:0;
      Vulkan.Descriptor_set.write descriptor_set Combined_image_sampler [texture] ~dst_binding:1 ~dst_array_element:0;
    ];
    { mapped; descriptor_set; pipeline_layout }
  in
  pipeline_layout, (input 0, input 1)

let set t ~geometry:(width, height) frame_number =
  let aspect = float height /. float width in
  Uniform_buffer_object.set t.mapped ~aspect frame_number

let bind t cmd =
  Vulkan.Cmd.bind_descriptor_sets cmd [t.descriptor_set]
    ~pipeline_bind_point:Graphics
    ~layout:t.pipeline_layout
    ~first_set:0
