module Vkt = Vk.Types
module A = Vulkan.A

let shader_code = [%blob "./slang.spv"]

let bindings = Vulkan.Binding.[
  v 0 Uniform_buffer         Vkt.Shader_stage_flags.vertex;
  v 1 Combined_image_sampler Vkt.Shader_stage_flags.fragment;
]

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

let no_stencil_op = Vkt.Stencil_op_state.make
    ~fail_op:Vkt.Stencil_op.Zero
    ~pass_op:Vkt.Stencil_op.Zero
    ~depth_fail_op:Vkt.Stencil_op.Zero
    ~compare_op:Vkt.Compare_op.Never
    ~compare_mask:0
    ~write_mask:0
    ~reference:0

let depth_testing = Vkt.Pipeline_depth_stencil_state_create_info.make ()
    ~depth_test_enable:true
    ~depth_write_enable:true
    ~depth_compare_op:Less
    ~depth_bounds_test_enable:false
    ~stencil_test_enable:false
    ~front:no_stencil_op
    ~back:no_stencil_op
    ~min_depth_bounds:0.0
    ~max_depth_bounds:0.0

let vertices = Vkt.Pipeline_vertex_input_state_create_info.make ()
    ~vertex_binding_descriptions:(Vkt.Vertex_input_binding_description.(array [
        make ~binding:0 ~stride:(Ctypes.sizeof Vertex.ctype) ~input_rate:Vertex;
      ]))
    ~vertex_attribute_descriptions:Vertex.attr_desc

let create ~sw ~device ~ubo ~render_pass (obj, texture_png) =
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
  let rasterizer = Vkt.Pipeline_rasterization_state_create_info.make ()
      ~depth_clamp_enable:false
      ~rasterizer_discard_enable:false
      ~polygon_mode:Fill
      ~line_width:1.0
      ~cull_mode:Vkt.Cull_mode_flags.back
      ~front_face:Clockwise
      ~depth_bias_enable:false
      ~depth_bias_constant_factor:0.0
      ~depth_bias_clamp:0.0
      ~depth_bias_slope_factor:0.0
  in
  let vertex_buffer = Vertices.allocate ~sw ~device obj in
  let command_pool = Vulkan.Cmd.create_pool ~sw device in
  let texture =
    let img = Texture.create ~sw ~command_pool ~device texture_png in
    let image_view = Vulkan.Image.create_view ~sw ~format:Texture.format ~device ~aspect_mask:Vkt.Image_aspect_flags.color img in
    Vkt.Descriptor_image_info.make ~image_view
      ~sampler:(sampler ~sw device)
      ~image_layout:Shader_read_only_optimal
  in
  let graphics_pipeline =
    let shader = Vulkan.Shader.load ~sw device shader_code in
    Vulkan.Pipeline.make ~sw ~device ~render_pass ()
      ~vertex_input_state:vertices
      ~stages:(Vkt.Pipeline_shader_stage_create_info.array [
          shader "vertMain" Vkt.Shader_stage_flags.vertex;
          shader "fragMain" Vkt.Shader_stage_flags.fragment;
        ])
      ~topology:Triangle_list
      ~rasterizer
      ~layout:pipeline_layout
      ~depth_stencil_state:depth_testing
  in
  Double.init (fun side ->
      let ubo = Double.get ubo side in
      let descriptor_set = A.get descriptor_sets (Double.to_index side) in
      Vulkan.Descriptor_set.update ~device [
        Vulkan.Descriptor_set.write descriptor_set Uniform_buffer         [ubo.Ubo.info] ~dst_binding:0 ~dst_array_element:0;
        Vulkan.Descriptor_set.write descriptor_set Combined_image_sampler [texture]      ~dst_binding:1 ~dst_array_element:0;
      ];
      fun cmd ->
        Vulkan.Cmd.bind_pipeline cmd ~stage:Graphics graphics_pipeline;
        Vulkan.Cmd.bind_descriptor_sets cmd [descriptor_set]
          ~pipeline_bind_point:Graphics
          ~layout:pipeline_layout
          ~first_set:0;
        Vertices.record vertex_buffer cmd;
    )
