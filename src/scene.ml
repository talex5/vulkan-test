module Vkt = Vk.Types
module A = Vulkan.A

let float_array = A.of_list Ctypes.float        (* Doesn't require GC protection *)

let shader_code = [%blob "./slang.spv"]

type t = {
  render_pass : Vkt.Render_pass.t;
  graphics_pipeline : Vkt.Pipeline.t;
  vertex_buffer : Vertices.t;
}

(* The render needs a colour image, which it clears and then writes to. *)
let render_pass format =
  let attachments = [
    (* The render needs a colour image, which it clears and then writes to: *)
    Vulkan.Attachment_description.make format
      ~load_op:Clear	(* Clear framebuffer before rendering *)
      ~store_op:Store
      ~initial_layout:Undefined
      ~final_layout:General;
    (* and a depth buffer so things near things are drawn in front of far things: *)
    Vulkan.Attachment_description.make D32_sfloat
      ~load_op:Clear
      ~store_op:Dont_care
      ~initial_layout:Undefined
      ~final_layout:Depth_stencil_attachment_optimal;
  ] in
  (* There's only a single subpass, using both: *)
  let subpass = Vulkan.Subpass.make Graphics
      ~color_attachments:[0, Color_attachment_optimal]
      ~depth_stencil_attachment:(1, Depth_attachment_stencil_read_only_optimal)
  in
  (* Subpass 0 depends on external inputs (not other subpasses): *)
  let dependency = Vkt.Subpass_dependency.make ()
      ~src_subpass:(Unsigned.UInt.to_int Vk.Const.subpass_external)
      ~dst_subpass:0
      ~src_stage_mask:Vkt.Pipeline_stage_flags.(color_attachment_output + late_fragment_tests)
      ~dst_stage_mask:Vkt.Pipeline_stage_flags.(color_attachment_output + early_fragment_tests)
      ~src_access_mask:Vkt.Access_flags.depth_stencil_attachment_write
      ~dst_access_mask:Vkt.Access_flags.(depth_stencil_attachment_write + color_attachment_write)
  in
  Vulkan.Render_pass.info ()
    ~attachments
    ~subpasses:[subpass]
    ~dependencies:[dependency]

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

let triangle_list = Vkt.Pipeline_input_assembly_state_create_info.make ()
    ~topology:Triangle_list
    ~primitive_restart_enable:false

let vertices = Vkt.Pipeline_vertex_input_state_create_info.make ()
    ~vertex_binding_descriptions:(Vkt.Vertex_input_binding_description.(array [
        make ~binding:0 ~stride:(Ctypes.sizeof Vertex.ctype) ~input_rate:Vertex;
      ]))
    ~vertex_attribute_descriptions:Vertex.attr_desc

let create ~sw ~format ~device (obj, texture_png) =
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
  let render_pass = Vulkan.Device.create_render_pass ~sw device (render_pass format) in
  let command_pool = Vulkan.Cmd.create_pool ~sw device in
  let texture =
    let img = Texture.create ~sw ~command_pool ~device texture_png in
    Vulkan.Image.create_view ~sw ~format ~device ~aspect_mask:Vkt.Image_aspect_flags.color img
  in
  let layout, inputs = Input.create ~sw ~device ~texture in
  let graphics_pipeline =
    let shader = Vulkan.Shader.load ~sw device shader_code in
    Vulkan.Pipeline.make ~sw ~device ~render_pass ()
      ~vertex_input_state:vertices
      ~stages:(Vkt.Pipeline_shader_stage_create_info.array [
          shader "vertMain" Vkt.Shader_stage_flags.vertex;
          shader "fragMain" Vkt.Shader_stage_flags.fragment;
        ])
      ~topology:Triangle_list
      ~primitive_restart_enable:true
      ~rasterizer
      ~layout
      ~depth_stencil_state:depth_testing
  in
  { render_pass; graphics_pipeline; vertex_buffer }, inputs

let record t input cmd framebuffer =
  let { Surface.framebuffer; geometry = (width, height); _ } = framebuffer in
  let black = Vkt.Clear_color_value.float_32 (float_array [0.0; 0.0; 0.0; 1.0]) in
  let far = Vkt.Clear_depth_stencil_value.make ~depth:1.0 ~stencil:0 in
  let clear_values = Vkt.Clear_value.array [
      Vkt.Clear_value.color black;
      Vkt.Clear_value.depth_stencil far;
    ] in
  let render_area = rect ~x:0 ~y:0 ~width ~height in
  let info = Vkt.Render_pass_begin_info.make ~render_pass:t.render_pass ~framebuffer ~render_area ~clear_values () in
  Vulkan.Cmd.render_pass cmd info ~subpass_contents:Inline (fun () ->
      Vulkan.Cmd.bind_pipeline cmd ~stage:Graphics t.graphics_pipeline;
      Vulkan.Cmd.set_viewport cmd ~first_viewport:0 [viewport ~width ~height];
      Vulkan.Cmd.set_scissor cmd ~first_scissor:0 [render_area];
      Input.bind input cmd;
      Vertices.record t.vertex_buffer cmd;
    )
