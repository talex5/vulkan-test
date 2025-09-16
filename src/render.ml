module Vkt = Vk.Types
module A = Vulkan.A
module Wl_buffer = Wayland.Wayland_client.Wl_buffer

(* The number of framebuffer images to allocate.
   This is the same number used by Mesa (WSI_WL_BUMPED_NUM_IMAGES) *)
let n_images = 4

let float_array = A.of_list Ctypes.float        (* Doesn't require GC protection *)

type t = {
  render_pass : Vkt.Render_pass.t;
  graphics_pipeline : Vkt.Pipeline.t;
  duo : Duo.t;
}

(* There is one of these for each allocated framebuffer *)
type frame_state = {
  mutable extent : Vkt.Extent_2d.t;			(* Size input for pipeline *)
  framebuffer : Vkt.Framebuffer.t;		        (* Buffer for output image *)
  buffer : [`V1] Wl_buffer.t;		                (* Corresponding Wayland object *)
  dma_buf_fd : Eio_unix.Fd.t;			        (* Corresponding Linux dmabuf *)
  render_finished : Vkt.Semaphore.t;	                (* Signalled when rendering is complete *)
  ctx : t;
}

let create ~sw ~device ~format =

  (* Shaders *)

  let load = Vulkan.Shader.load ~sw device [%blob "./slang.spv"] in
  let shader_stages = Vkt.Pipeline_shader_stage_create_info.array [
      load "vertMain" Vkt.Shader_stage_flags.vertex;
      load "fragMain" Vkt.Shader_stage_flags.fragment;
    ]
  in

  (* Vertex information (none) *)

  let vertex_input_info = Vkt.Pipeline_vertex_input_state_create_info.make () in
  let input_assembly = Vkt.Pipeline_input_assembly_state_create_info.make ()
      ~topology:Triangle_list
      ~primitive_restart_enable:false
  in

  (* Viewport and scissors (values are set dynamically later) *)

  let viewport = Vkt.Viewport.make
      ~x:0.0
      ~y:0.0
      ~width:0.0
      ~height:0.0
      ~min_depth:0.0
      ~max_depth:1.0
  in
  let scissor = Vkt.Rect_2d.make
      ~offset:(Vkt.Offset_2d.make ~x:0 ~y:0)
      ~extent:(Vkt.Extent_2d.make ~width:0 ~height:0)
  in
  let viewport_state = Vkt.Pipeline_viewport_state_create_info.make ()
      ~viewports:(Vkt.Viewport.array [viewport])
      ~scissors:(Vkt.Rect_2d.array [scissor])
  in

  (* Rendering *)

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
  let multisampling = Vkt.Pipeline_multisample_state_create_info.make ()
      ~sample_shading_enable:false
      ~rasterization_samples:Vkt.Sample_count_flags.n1
      ~min_sample_shading:0.0
      ~alpha_to_coverage_enable:false
      ~alpha_to_one_enable:false
  in
  let color_blend_attachment =
    Vkt.Pipeline_color_blend_attachment_state.make ()
      ~color_write_mask:Vkt.Color_component_flags.(r + g + b + a)
      ~blend_enable:false
      ~src_color_blend_factor:One
      ~dst_color_blend_factor:Zero
      ~color_blend_op:Add
      ~src_alpha_blend_factor:One
      ~dst_alpha_blend_factor:Zero
      ~alpha_blend_op:Add
  in
  let color_blending = Vkt.Pipeline_color_blend_state_create_info.make ()
      ~logic_op_enable:false
      ~attachments:(Vkt.Pipeline_color_blend_attachment_state.array [color_blend_attachment])
      ~logic_op:Copy
      ~blend_constants:(float_array [0.; 0.; 0.; 0.])
  in

  let color_attachment = Vkt.Attachment_description.make ()
      ~format:format
      ~samples:Vkt.Sample_count_flags.n1
      ~load_op:Clear	(* Clear framebuffer before rendering *)
      ~store_op:Store
      ~stencil_load_op:Dont_care
      ~stencil_store_op:Dont_care
      ~initial_layout:Undefined
      ~final_layout:General
  in
  let color_attachment_ref = Vkt.Attachment_reference.make
      ~attachment:0
      ~layout:Color_attachment_optimal
  in
  let subpass = Vkt.Subpass_description.make ()
      ~pipeline_bind_point:Graphics
      ~color_attachments:(Vkt.Attachment_reference.array [color_attachment_ref])
  in
  let dependency = Vkt.Subpass_dependency.make ()
      ~src_subpass:(Unsigned.UInt.to_int Vk.Const.subpass_external)
      ~dst_subpass:0
      ~src_stage_mask:Vkt.Pipeline_stage_flags.color_attachment_output
      ~src_access_mask:Vkt.Access_flags.empty
      ~dst_stage_mask:Vkt.Pipeline_stage_flags.color_attachment_output
      ~dst_access_mask:Vkt.Access_flags.color_attachment_write
  in
  let create_info = Vkt.Render_pass_create_info.make ()
      ~attachments:(Vkt.Attachment_description.array [color_attachment])
      ~subpasses:(Vkt.Subpass_description.array [subpass])
      ~dependencies:(Vkt.Subpass_dependency.array [dependency])
  in
  let render_pass = Vulkan.Device.create_render_pass ~sw device create_info in

  let dynamic_states = A.of_list Vkt.Dynamic_state.ctype [ Viewport; Scissor ] in
  let dynamic_state = Vkt.Pipeline_dynamic_state_create_info.make ~dynamic_states () in

  let layout, inputs = Input.create ~sw ~device in
  let command_pool = Vulkan.Cmd.create_pool ~sw device in
  let duo = Duo.make ~sw ~command_pool inputs in

  let pipeline_info = Vkt.Graphics_pipeline_create_info.make ()
      ~stages:shader_stages
      ~vertex_input_state:vertex_input_info
      ~input_assembly_state:input_assembly
      ~viewport_state
      ~rasterization_state:rasterizer
      ~multisample_state:multisampling
      ~color_blend_state:color_blending
      ~dynamic_state
      ~layout
      ~render_pass:render_pass
      ~subpass:0
      ~base_pipeline_index:0
  in
  let graphics_pipeline = Vulkan.Device.create_pipeline ~sw device pipeline_info in
  { render_pass; graphics_pipeline; duo }

let record_commands frame_state ~frame:frame_number =
  let t = frame_state.ctx in
  let extent = frame_state.extent in
  let framebuffer = frame_state.framebuffer in
  let { Duo.input; command_buffer } = Duo.next t.duo in
  Vulkan.Cmd.reset command_buffer;
  Vulkan.Cmd.record command_buffer (fun () ->
      let black = Vkt.Clear_color_value.float_32 (float_array [0.0; 0.0; 0.0; 1.0]) in
      let clear_values = Vkt.Clear_value.array [Vkt.Clear_value.color black] in
      let offset = Vkt.Offset_2d.make ~x:0 ~y:0 in
      let render_area = Vkt.Rect_2d.make ~offset ~extent in
      let render_pass_info = Vkt.Render_pass_begin_info.make ~render_pass:t.render_pass ~framebuffer ~render_area ~clear_values () in
      Vulkan.Cmd.render_pass command_buffer render_pass_info ~subpass_contents:Inline (fun () ->
          Vulkan.Cmd.bind_pipeline command_buffer ~stage:Graphics t.graphics_pipeline;
          let viewport = Vkt.Viewport.make
              ~x:0.0
              ~y:0.0
              ~width:(float @@ Vkt.Extent_2d.width extent)
              ~height:(float @@ Vkt.Extent_2d.height extent)
              ~min_depth:0.0
              ~max_depth:1.0
          in
          Vulkan.Cmd.set_viewport command_buffer ~first_viewport:0 [viewport];
          let scissor = Vkt.Rect_2d.make ~offset ~extent in
          Vulkan.Cmd.set_scissor command_buffer ~first_scissor:0 [scissor];
          Input.set input command_buffer frame_number;
          Vulkan.Cmd.draw command_buffer ~vertex_count:3 ~instance_count:1 ~first_vertex:0 ~first_instance:0;
        );
    );
  command_buffer
