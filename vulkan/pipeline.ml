open Common

let viewport ~width ~height =
  Vkt.Viewport.make
    ~x:0.0
    ~y:0.0
    ~width:(float width)
    ~height:(float height)
    ~min_depth:0.0
    ~max_depth:1.0

let rect ~x ~y ~width ~height =
  let offset = Vkt.Offset_2d.make ~x ~y in
  let extent = Vkt.Extent_2d.make ~width ~height in
  Vkt.Rect_2d.make ~offset ~extent

(* The real values can be set dynamically later *)
let dummy_viewport_state =
  Vkt.Pipeline_viewport_state_create_info.make ()
    ~viewports:(Vkt.Viewport.array [viewport ~width:0 ~height:0])
    ~scissors:(Vkt.Rect_2d.array [rect ~x:0 ~y:0 ~width:0 ~height:0])

(* When the fragment shader outputs a pixel, it replaces whatever was already there in the framebuffer. *)
let no_colour_blending =
  let no_blend =
    Vkt.Pipeline_color_blend_attachment_state.make ()
      ~blend_enable:false
      ~color_write_mask:Vkt.Color_component_flags.(r + g + b + a)
      ~src_color_blend_factor:One
      ~dst_color_blend_factor:Zero
      ~color_blend_op:Add
      ~src_alpha_blend_factor:One
      ~dst_alpha_blend_factor:Zero
      ~alpha_blend_op:Add
  in
  Vkt.Pipeline_color_blend_state_create_info.make ()
    ~logic_op_enable:false
    ~attachments:(Vkt.Pipeline_color_blend_attachment_state.array [no_blend])
    ~logic_op:Copy
    ~blend_constants:(float_array [0.; 0.; 0.; 0.])

let dynamic_viewport_and_scissor =
  let dynamic_states = A.of_list Vkt.Dynamic_state.ctype [ Viewport; Scissor ] in
  Vkt.Pipeline_dynamic_state_create_info.make ~dynamic_states ()

let no_multisampling = Vkt.Pipeline_multisample_state_create_info.make ()
    ~sample_shading_enable:false
    ~rasterization_samples:Vkt.Sample_count_flags.n1
    ~min_sample_shading:0.0
    ~alpha_to_coverage_enable:false
    ~alpha_to_one_enable:false

let no_stencil_op = Vkt.Stencil_op_state.make
    ~fail_op:Vkt.Stencil_op.Zero
    ~pass_op:Vkt.Stencil_op.Zero
    ~depth_fail_op:Vkt.Stencil_op.Zero
    ~compare_op:Vkt.Compare_op.Never
    ~compare_mask:0
    ~write_mask:0
    ~reference:0

let no_input_vertices = Vkt.Pipeline_vertex_input_state_create_info.make ()

let no_depth_testing = Vkt.Pipeline_depth_stencil_state_create_info.make ()
    ~depth_test_enable:false
    ~depth_write_enable:false
    ~depth_compare_op:Less
    ~depth_bounds_test_enable:false
    ~stencil_test_enable:false
    ~front:no_stencil_op
    ~back:no_stencil_op
    ~min_depth_bounds:0.0
    ~max_depth_bounds:0.0

let make
    ?(vertex_input_state=no_input_vertices)
    ?(viewport_state=dummy_viewport_state)
    ?(multisample_state=no_multisampling)
    ?(color_blend_state=no_colour_blending)
    ?(dynamic_state=dynamic_viewport_and_scissor)
    ?(subpass=0)
    ?(base_pipeline_index=0)
    ?(topology=Vkt.Primitive_topology.Triangle_strip)
    ?(primitive_restart_enable=false)
    ?(depth_stencil_state=no_depth_testing)
    ~sw ~device ~render_pass
    ~stages ~rasterizer ~layout
    () =
  let input_assembly_state = Vkt.Pipeline_input_assembly_state_create_info.make ()
      ~topology
      ~primitive_restart_enable
  in
  Device.create_pipeline ~sw device @@
  Vkt.Graphics_pipeline_create_info.make ()
    ~layout
    ~stages
    ~vertex_input_state
    ~input_assembly_state
    ~viewport_state
    ~rasterization_state:rasterizer
    ~multisample_state
    ~color_blend_state
    ~dynamic_state
    ~render_pass
    ~subpass
    ~base_pipeline_index
    ~depth_stencil_state
