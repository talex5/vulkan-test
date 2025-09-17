module Vkt = Vk.Types
module A = Vulkan.A

(* Data shared with the shader code. *)
module Uniform_buffer_object = struct
  type mark
  type t = mark Ctypes.structure
  let ctype : t Ctypes.typ = Ctypes.structure "Uniform_buffer_object"
  let dist = Ctypes.field ctype "dist" Ctypes.float
  let () = Ctypes.seal ctype
  let size = Ctypes.sizeof ctype
end

let bindings = Vulkan.Binding.[
  v 0 Uniform_buffer Vkt.Shader_stage_flags.vertex;
]

type t = {
  mapped : Uniform_buffer_object.t;             (* Input data *)
  descriptor_set : Vkt.Descriptor_set.t;        (* How to use it in the pipeline *)
  pipeline_layout : Vkt.Pipeline_layout.t       (* Metadata about the input format *)
}

let create ~sw ~device =
  let max_sets = 2 in
  let layout = Vulkan.Descriptor_set.make_layout ~sw device bindings in
  let pipeline_layout =
    Vulkan.Device.create_pipeline_layout ~sw device @@
    Vkt.Pipeline_layout_create_info.make ()
      ~set_layouts:(Vkt.Descriptor_set_layout.array [layout])
  in
  let pool = Vulkan.Descriptor_set.create_pool ~sw device ~max_sets [
      Uniform_buffer, max_sets;
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
    let whole_buffer = Vkt.Descriptor_buffer_info.make ~buffer:ubo.buffer ~offset:Vkt.Device_size.zero ~range () in
    Vulkan.Descriptor_set.update device ~writes:[
      Vulkan.Descriptor_set.write descriptor_set Uniform_buffer [whole_buffer] ~dst_binding:0 ~dst_array_element:0;
    ];
    { mapped; descriptor_set; pipeline_layout }
  in
  pipeline_layout, (input 0, input 1)

let set t cmd frame_number =
  Ctypes.setf t.mapped Uniform_buffer_object.dist @@ Float.of_int ((frame_number land 0xff) - 100) /. 100.;
  Vulkan.Cmd.bind_descriptor_sets cmd [t.descriptor_set]
    ~pipeline_bind_point:Graphics
    ~layout:t.pipeline_layout
    ~first_set:0
