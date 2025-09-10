open Helpers

module Wl_buffer = Wayland.Wayland_client.Wl_buffer

(* The number of framebuffer images to allocate.
   This is the same number used by Mesa (WSI_WL_BUMPED_NUM_IMAGES) *)
let n_images = 4

module Uniform_buffer_object = struct
  type mark
  type t = mark Ctypes.structure
  let view : t Ctypes.typ = Ctypes.structure "Uniform_buffer_object"
  let ctype = view
  let dist = Ctypes.field ctype "dist" Ctypes.float
  let () = Ctypes.seal ctype
end

type render_state = {
  render_pass : Vkt.Render_pass.t;
  graphics_pipeline : Vkt.Pipeline.t;
  pipeline_layout : Vkt.Pipeline_layout.t;
}

(* There is one of these for each allocated framebuffer *)
type frame_state = {
  uniform_buffer_mapped : Uniform_buffer_object.t;      (* Input arguments for shaders *)
  descriptor_set : Vkt.Descriptor_set.t;	        (* Says to pass uniform_buffer_mapped to shaders *)
  mutable extent : Vkt.Extent_2d.t;			(* Size input for pipeline *)
  framebuffer : Vkt.Framebuffer.t;		        (* Buffer for output image *)
  buffer : [`V1] Wl_buffer.t;		                (* Corresponding Wayland object *)
  dma_buf_fd : Unix.file_descr;			        (* Corresponding Linux dmabuf *)
  render_finished_semaphore : Vkt.Semaphore.t;	        (* Signalled when rendering is complete *)
  ctx : render_state;		                        (* Holds render_pass, pipeline_layout and graphics_pipeline *)
}

let create ~physical_device ~device ~format =

  (* Shaders *)

  let vert_shader_module = load_shader_module device [%blob "./vert.spv"] in
  let frag_shader_module = load_shader_module device [%blob "./frag.spv"] in

  let vert_shader_stage_info = Vkt.Pipeline_shader_stage_create_info.make ()
      ~stage:Vkt.Shader_stage_flags.vertex
      ~module':vert_shader_module
      ~name:"main"
  in
  let frag_shader_stage_info = Vkt.Pipeline_shader_stage_create_info.make ()
      ~stage:Vkt.Shader_stage_flags.fragment
      ~module':frag_shader_module
      ~name:"main"
  in
  let shader_stages = Vkt.Pipeline_shader_stage_create_info.array [vert_shader_stage_info; frag_shader_stage_info] in

  (* Pipeline input data *)

  log "vkCreateDescriptorSetLayout";
  let ubo_layout_binding = Vkt.Descriptor_set_layout_binding.make ()
      ~binding:0
      ~descriptor_type:Uniform_buffer
      ~descriptor_count:1
      ~stage_flags:Vkt.Shader_stage_flags.vertex
  in
  let create_info = Vkt.Descriptor_set_layout_create_info.make ()
      ~bindings:(Vkt.Descriptor_set_layout_binding.array [ubo_layout_binding])
  in
  let descriptor_set_layout = Vkc.create_descriptor_set_layout ~device ~create_info () <?> "create_descriptor_set_layout" in

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
  let create_info = Vkt.Pipeline_layout_create_info.make ()
      ~set_layouts:(Vkt.Descriptor_set_layout.array [descriptor_set_layout])
  in
  log "VkCreatePipelineLayout";
  let pipeline_layout = Vkc.create_pipeline_layout ~device ~create_info () <?> "create_pipeline_layout" in

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
  log "vkCreateRenderPass";
  let render_pass = Vkc.create_render_pass ~device ~create_info () <?> "create_render_pass" in

  let dynamic_states = A.of_list Vkt.Dynamic_state.ctype [ Viewport; Scissor ] in
  let dynamic_state = Vkt.Pipeline_dynamic_state_create_info.make ~dynamic_states:dynamic_states () in

  let memory_properties = Vkc.get_physical_device_memory_properties physical_device in

  let size = Vkt.Device_size.of_int (Ctypes.sizeof Uniform_buffer_object.ctype) in

  let pool_size = Vkt.Descriptor_pool_size.make
      ~typ:Uniform_buffer
      ~descriptor_count:n_images
  in

  let create_info = Vkt.Descriptor_pool_create_info.make ()
      ~pool_sizes:(Vkt.Descriptor_pool_size.array [pool_size])
      ~max_sets:n_images
  in
  log "vkCreateDescriptorPool";
  let descriptor_pool = Vkc.create_descriptor_pool ~device ~create_info () <?> "create_descriptor_pool" in

  let layouts = Vkt.Descriptor_set_layout.array (List.init n_images (Fun.const descriptor_set_layout)) in
  let allocate_info = Vkt.Descriptor_set_allocate_info.make ()
      ~descriptor_pool
      ~set_layouts:layouts
  in
  log "vkAllocateDescriptorSets";
  let descriptor_sets = Vkc.allocate_descriptor_sets ~device ~allocate_info <?> "allocate_descriptor_sets" in

  let inputs = Array.init n_images (fun i ->
      log "Create uniform buffer %d" i;

      log "create_buffer";
      let buffer, memory = create_buffer ~device ~memory_properties size
          ~usage:Vkt.Buffer_usage_flags.uniform_buffer
          ~properties:Vkt.Memory_property_flags.(host_visible + host_coherent)
      in
      log "vkMapMemory";
      let mapped =
        Vkc.map_memory memory ~device ~offset:Vkt.Device_size.zero ~size ~flags:Vkt.Memory_map_flags.empty ()
        <?> "map_memory"
      in

      let buffer_info = Vkt.Descriptor_buffer_info.make ~buffer ~offset:Vkt.Device_size.zero ~range:size () in
      let descriptor_set = A.get descriptor_sets i in
      let descriptor_write = Vkt.Write_descriptor_set.make ()
          ~dst_set:descriptor_set
          ~dst_binding:0
          ~dst_array_element:0
          ~descriptor_type:Uniform_buffer
          ~descriptor_count:1
          ~buffer_info:(Vkt.Descriptor_buffer_info.array [buffer_info])
          ~image_info:(Vkt.Descriptor_image_info.array [])
          ~texel_buffer_view:(Vkt.Buffer_view.array [])
      in
      log "vkUpdateDescriptorSets";
      Vkc.update_descriptor_sets ()
        ~device
        ~descriptor_writes:(Vkt.Write_descriptor_set.array [descriptor_write]);
      let mapped = Ctypes.(!@ (from_voidp Uniform_buffer_object.ctype mapped)) in
      (mapped, descriptor_set)
    ) in

  let pipeline_info = Vkt.Graphics_pipeline_create_info.make ()
      ~stages:shader_stages
      ~vertex_input_state:vertex_input_info
      ~input_assembly_state:input_assembly
      ~viewport_state
      ~rasterization_state:rasterizer
      ~multisample_state:multisampling
      ~color_blend_state:color_blending
      ~dynamic_state
      ~layout:pipeline_layout
      ~render_pass:render_pass
      ~subpass:0
      ~base_pipeline_index:0
  in
  log "vkCreateGraphicsPipeline";

  let create_infos = Vkt.Graphics_pipeline_create_info.array [pipeline_info] in
  let pipelines = Vkc.create_graphics_pipelines () ~device ~create_infos <?> "create_graphics_pipelines" in
  match A.to_list pipelines with
  | [ graphics_pipeline ] -> { render_pass; graphics_pipeline; pipeline_layout }, inputs
  | _ -> failwith "create_graphics_pipelines: wrong number of pipelines!"

let record_commands input ~frame:frame_number command_buffer =
  let t = input.ctx in
  let extent = input.extent in
  let framebuffer = input.framebuffer in
  Ctypes.setf input.uniform_buffer_mapped Uniform_buffer_object.dist @@ Float.of_int ((frame_number land 0xff) - 100) /. 100.;

  Vkc.reset_command_buffer ~command_buffer ~flags:Vkt.Command_buffer_reset_flags.empty () <?> "reset_command_buffer";

  let begin_info = Vkt.Command_buffer_begin_info.make () in
  Vkc.begin_command_buffer ~command_buffer ~begin_info <?> "begin_command_buffer";

  let black = Vkt.Clear_color_value.float_32 (float_array [0.0; 0.0; 0.0; 1.0]) in
  let clear_values = Vkt.Clear_value.array [Vkt.Clear_value.color black] in
  let offset = Vkt.Offset_2d.make ~x:0 ~y:0 in
  let render_area = Vkt.Rect_2d.make ~offset ~extent in
  let render_pass_info = Vkt.Render_pass_begin_info.make ~render_pass:t.render_pass ~framebuffer ~render_area ~clear_values () in
  Vkc.cmd_begin_render_pass command_buffer render_pass_info Vkt.Subpass_contents.Inline;
  Vkc.cmd_bind_pipeline command_buffer Vkt.Pipeline_bind_point.Graphics t.graphics_pipeline;
  let viewport = Vkt.Viewport.make
      ~x:0.0
      ~y:0.0
      ~width:(float @@ Vkt.Extent_2d.width extent)
      ~height:(float @@ Vkt.Extent_2d.height extent)
      ~min_depth:0.0
      ~max_depth:1.0
  in
  Vkc.cmd_set_viewport command_buffer ~first_viewport:0 ~viewports:(Vkt.Viewport.array [viewport]);
  let scissor = Vkt.Rect_2d.make ~offset ~extent in
  Vkc.cmd_set_scissor command_buffer ~first_scissor:0 ~scissors:(Vkt.Rect_2d.array [scissor]);

  Vkc.cmd_bind_descriptor_sets command_buffer (Vkt.Descriptor_set.array [input.descriptor_set]) ()
    ~pipeline_bind_point:Graphics
    ~layout:t.pipeline_layout
    ~first_set:0;

  Vkc.cmd_draw command_buffer 3 1 0 0;

  Vkc.cmd_end_render_pass command_buffer;

  Vkc.end_command_buffer ~command_buffer <?> "end_command_buffer"
