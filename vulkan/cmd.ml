open Common

type pool = {
  device : Device.t;
  command_pool : Vkt.Command_pool.t;
}

let create_pool ~sw device =
  let create_info = Vkt.Command_pool_create_info.make ()
      ~flags:Vkt.Command_pool_create_flags.reset_command_buffer
      ~queue_family_index:device.Device.graphics_family
  in
  let command_pool = Vkc.create_command_pool () ~device:(Device.dev device) ~create_info <?> "create_command_pool" in
  Switch.on_release sw (fun () -> Vkc.destroy_command_pool (Device.dev device) (Some command_pool) None);
  { device; command_pool }

let allocate_buffer ~sw pool =
  let allocate_info = Vkt.Command_buffer_allocate_info.make ()
      ~command_pool:pool.command_pool
      ~level:Primary
      ~command_buffer_count:1
  in
  let device = pool.device.device in
  let command_buffers = Vkc.allocate_command_buffers ~device ~allocate_info <?> "allocate_command_buffers" in
  Switch.on_release sw (fun () -> Vkc.free_command_buffers ~device ~command_pool:pool.command_pool command_buffers);
  match A.to_list command_buffers with
  | [x] -> x
  | _ -> failwith "Wrong number of command buffers!"

let submit device ?(wait=[]) ?(signal_semaphores=[]) ?fence command_buffer =
  let wait_semaphores = Vkt.Semaphore.array (List.map fst wait) in
  let wait_dst_stage_mask = A.of_list Vkt.Pipeline_stage_flags.ctype (List.map snd wait) in
  let signal_semaphores = Vkt.Semaphore.array signal_semaphores in
  let submits = Vkt.Submit_info.array [
      Vkt.Submit_info.make ()
        ~wait_semaphores
        ~wait_dst_stage_mask
        ~command_buffers:(Vkt.Command_buffer.array [command_buffer])
        ~signal_semaphores
    ] in
  Vkc.queue_submit device.Device.graphics_queue ~submits ?fence () <?> "queue_submit"

let reset command_buffer =
  Vkc.reset_command_buffer ~command_buffer ~flags:Vkt.Command_buffer_reset_flags.empty () <?> "reset_command_buffer"

let record ?flags command_buffer fn =
  let begin_info = Vkt.Command_buffer_begin_info.make ?flags () in
  Vkc.begin_command_buffer ~command_buffer ~begin_info <?> "begin_command_buffer";
  fn ();
  Vkc.end_command_buffer ~command_buffer <?> "end_command_buffer"

let render_pass command_buffer render_pass_info ~subpass_contents fn =
  Vkc.cmd_begin_render_pass command_buffer render_pass_info subpass_contents;
  fn ();
  Vkc.cmd_end_render_pass command_buffer

let bind_pipeline ~stage t pipeline = Vkc.cmd_bind_pipeline t stage pipeline

let set_viewport t ~first_viewport viewports =
  Vkc.cmd_set_viewport t ~first_viewport ~viewports:(Vkt.Viewport.array viewports)

let set_scissor t ~first_scissor scissors =
  Vkc.cmd_set_scissor t ~first_scissor ~scissors:(Vkt.Rect_2d.array scissors)

let bind_descriptor_sets ~pipeline_bind_point ~layout ~first_set t sets =
  Vkc.cmd_bind_descriptor_sets t (Vkt.Descriptor_set.array sets) ()
    ~pipeline_bind_point
    ~layout
    ~first_set

let draw t ~first_vertex ~first_instance ~vertex_count ~instance_count =
  Vkc.cmd_draw t vertex_count instance_count first_vertex first_instance
