open Eio.Std
module Vkt = Vk.Types
module A = Vulkan.A
module Wl_buffer = Wayland.Wayland_client.Wl_buffer
module Swap_chain = Vulkan.Swap_chain

let float_array = A.of_list Ctypes.float        (* Doesn't require GC protection *)

type t = {
  device : Vulkan.Device.t;
  format : Vkt.Format.t;
  render_pass : Vkt.Render_pass.t;
  graphics_pipeline : Vkt.Pipeline.t;
  duo : Duo.t;
  redraw_needed : Eio.Condition.t;
  mutable frame : int;
  window : Window.t;
  in_flight_fence : Vkt.Fence.t;                (* Signalled when GPU finishes rendering pipeline *)
  image_available : Vkt.Semaphore.t;            (* Signalled when the compositer has finished showing the old image *)
}

let create_pipeline ~sw ~format device =

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
  render_pass, graphics_pipeline, inputs

let record_commands t ~geometry:(width, height) framebuffer =
  let extent = Vkt.Extent_2d.make ~width ~height in
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
          Input.set input command_buffer t.frame;
          Vulkan.Cmd.draw command_buffer ~vertex_count:3 ~instance_count:1 ~first_vertex:0 ~first_instance:0;
        );
    );
  command_buffer

let next_as_promise cond =
  let p, r = Promise.create () in
  ignore (Eio.Condition.register_immediate cond (Promise.resolve r) : Eio.Condition.request);
  p

let create_framebuffer t =
  Vulkan.Image.create_framebuffer ~device:t.device ~format:t.format ~render_pass:t.render_pass

(* Note: Mesa's WSI also calls set_memory_ownership when acquiring and presenting images.
   I'm not sure what that's for (it doesn't do anything on my GPU) so I skipped it. *)
let submit_frame t (fb : Swap_chain.frame) command_buffer =
  (* If we're still rendering the last frame, wait for that to finish.
     Needed because e.g. [image_available_semaphore] isn't per-framebuffer. *)
  let device = t.device in
  Vulkan.Fence.wait device [t.in_flight_fence];
  Vulkan.Fence.reset device [t.in_flight_fence];
  (* At this point, [image_available_semaphore] is no longer in use,
     because the pipeline waited on it and reset it before finishing,
     which trigged inFlightFence. *)

  (* Get the semaphore that the compositor's render job will signal when
     it's done reading the image: *)
  Vulkan.Semaphore.import device fb.dma_buf_fd t.image_available;

  Vulkan.Cmd.submit device command_buffer
    ~wait:[t.image_available, Vkt.Pipeline_stage_flags.color_attachment_output]
    ~signal_semaphores:[fb.render_finished]
    ~fence:t.in_flight_fence;

  (* Attach [render_finished] to the dmabuf so that the compositor doesn't
     start displaying the image until the GPU has finished rendering it. *)
  Vulkan.Semaphore.export device fb.render_finished fb.dma_buf_fd;
  (* Note: draw_frame waits for the previous frame to finish, so the other input is now free *)
  Window.attach t.window ~buffer:fb.wl_buffer

let render_loop t ~make_swap_chain =
  while true do
    let geometry = Window.geometry t.window in
    Switch.run @@ fun sw ->
    let framebuffers = make_swap_chain ~sw geometry (create_framebuffer ~sw t) in
    while geometry = Window.geometry t.window do
      let fb = Swap_chain.get_framebuffer framebuffers in
      let redraw_needed = next_as_promise t.redraw_needed in
      record_commands t ~geometry fb.framebuffer |> submit_frame t fb;
      Promise.await redraw_needed
    done
  done

let trigger_redraw t =
  Eio.Condition.broadcast t.redraw_needed

let create ~sw ~device ~window =
  let format = Vkt.Format.B8g8r8a8_srgb in
  let render_pass, graphics_pipeline, inputs = create_pipeline ~sw ~format device in
  let command_pool = Vulkan.Cmd.create_pool ~sw device in
  let duo = Duo.make ~sw ~command_pool inputs in
  let redraw_needed = Eio.Condition.create () in
  let dmabuf = window.Window.wayland_dmabuf in
  let make_swap_chain = Vulkan.Swap_chain.create ~dmabuf ~device ~format in
  let t = {
    device; format; window; render_pass; graphics_pipeline; duo; redraw_needed; frame = 0;
    in_flight_fence = Vulkan.Fence.create ~sw device Vkt.Fence_create_flags.signaled;
    image_available = Vulkan.Semaphore.create ~sw device;
  } in
  Fiber.fork_daemon ~sw (fun () -> render_loop t ~make_swap_chain);
  t
