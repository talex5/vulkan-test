open Common

let create
    ?(image_type=Vkt.Image_type.N2d)
    ?(samples=Vkt.Sample_count_flags.n1)
    ?(mip_levels=1)
    ?(array_layers=1)
    ?handle_types
    ~sw
    ~extent ~flags ~tiling ~usage ~sharing_mode ~initial_layout
    ~format device =
  let next =
    handle_types |> Option.map (fun handle_types ->
        let view_formats = A.of_list Vkt.Format.ctype [format] in
        let formats = Vkt.Image_format_list_create_info.make ~view_formats () in
        let extern =
          Vkt.External_memory_image_create_info.make ()
            ~handle_types
            ~next:(ext (Vkt.Image_format_list_create_info.addr formats))
        in
        ext (Vkt.External_memory_image_create_info.addr extern)
      )
  in
  let create_info = Vkt.Image_create_info.make ?next ()
      ~flags ~image_type ~mip_levels ~array_layers ~format ~extent
      ~samples ~tiling ~usage ~sharing_mode ~initial_layout
  in
  let t = Vkc.create_image ~device:(Device.dev device) ~create_info () <?> "create_image" in
  Switch.on_release sw (fun () -> Vkc.destroy_image (Device.dev device) (Some t) None);
  t

(* Allocate some device memory for the image *)
let allocate_image_memory ?handle_types ~properties ~sw ~device image =
  let reqs = Vkc.get_image_memory_requirements ~device:(Device.dev device) image in
  let next =
    handle_types |> Option.map (fun handle_types ->
        let memory_export_info = Vkt.Export_memory_allocate_info.make ~handle_types () in
        let memory_dedicated_info = Vkt.Memory_dedicated_allocate_info.make ()
            ~next:(ext (Vkt.Export_memory_allocate_info.addr memory_export_info))
            ~image
        in
        ext (Vkt.Memory_dedicated_allocate_info.addr memory_dedicated_info)
      )
  in
  let allocate_info =
    Vkt.Memory_allocate_info.make ?next ()
      ~allocation_size:(Vkt.Memory_requirements.size reqs)
      ~memory_type_index:(
        Device.find_memory_type device
          ~type_filter:(Vkt.Memory_requirements.memory_type_bits reqs)
          ~properties
      )
  in
  let memory = Vkc.allocate_memory ~device:(Device.dev device) ~allocate_info () <?> "allocate_memory" in
  Switch.on_release sw (fun () -> Vkc.free_memory (Device.dev device) (Some memory) None);
  (* Attach the memory to the image *)
  let memory_offset = Vkt.Device_size.zero in
  Vkc.bind_image_memory ~device:(Device.dev device) image memory ~memory_offset <?> "bind_image_memory";
  memory

let get_memory_fd ~sw device memory =
  let get_fd_info = Vkt.Memory_get_fd_info_khr.make ()
      ~memory
      ~handle_type:Vkt.External_memory_handle_type_flags.opaque_fd
  in
  let module E = (val device.Device.ext) in
  let x : int = E.get_memory_fd_khr get_fd_info in
  Eio_unix.Fd.of_unix ~sw ~close_unix:true (Obj.magic x : Unix.file_descr)

let get_layout ~device t =
  let subresource = Vkt.Image_subresource.make
      ~aspect_mask:Vkt.Image_aspect_flags.color
      ~mip_level:0
      ~array_layer:0
  in
  Vkc.get_image_subresource_layout ~device:(Device.dev device) t subresource

let create_view ~aspect_mask ~sw ~format ~device image =
  let components = Vkt.Component_mapping.make ~r:Identity ~g:Identity ~b:Identity ~a:Identity in
  let subresource_range = Vkt.Image_subresource_range.make
      ~aspect_mask
      ~base_mip_level:0
      ~level_count:1
      ~base_array_layer:0
      ~layer_count:1
  in
  let create_info =
    Vkt.Image_view_create_info.make ()
      ~image
      ~view_type:N2d
      ~format
      ~components
      ~subresource_range
  in
  let v = Vkc.create_image_view ~device:(Device.dev device) ~create_info () <?> "create_image_view" in
  Switch.on_release sw (fun () -> Vkc.destroy_image_view (Device.dev device) (Some v) None);
  v

let create_framebuffer ?depth_buffer ~sw ~device ~format ~render_pass (width, height) image =
  let view = create_view ~sw ~format ~device ~aspect_mask:Vkt.Image_aspect_flags.color image in
  let attachments = Vkt.Image_view.array ([view] @ Option.to_list depth_buffer) in
  let create_info = Vkt.Framebuffer_create_info.make ()
      ~render_pass
      ~attachments
      ~width
      ~height
      ~layers:1
  in
  let fb = Vkc.create_framebuffer ~device:(Device.dev device) ~create_info () <?> "create_framebuffer" in
  Switch.on_release sw (fun () -> Device.wait_idle device; Vkc.destroy_framebuffer (Device.dev device) (Some fb) None);
  fb

let transition_layout ~command_pool ~old_layout ~new_layout t =
  Cmd.run_one_time command_pool @@ fun command_buffer ->
  let subresource_range =
    Vkt.Image_subresource_range.make
      ~aspect_mask:Vkt.Image_aspect_flags.color
      ~base_mip_level:0
      ~level_count:1
      ~base_array_layer:0
      ~layer_count:1
  in
  let barrier = Vkt.Image_memory_barrier.make ()
      ~image:t
      ~old_layout
      ~new_layout
      ~src_queue_family_index:(Unsigned.UInt.to_int Vk.Const.queue_family_ignored)
      ~dst_queue_family_index:(Unsigned.UInt.to_int Vk.Const.queue_family_ignored)
      ~subresource_range
  in
  match old_layout, new_layout with
  | Undefined, Transfer_dst_optimal ->
    let barrier = barrier
        ~src_access_mask:Vkt.Access_flags.empty
        ~dst_access_mask:Vkt.Access_flags.transfer_write
    in
    Vkc.cmd_pipeline_barrier command_buffer
      ~src_stage_mask:Vkt.Pipeline_stage_flags.top_of_pipe
      ~dst_stage_mask:Vkt.Pipeline_stage_flags.transfer
      ~image_memory_barriers:(Vkt.Image_memory_barrier.array [barrier]) ();
  | Transfer_dst_optimal, Vkt.Image_layout.Shader_read_only_optimal ->
    let barrier = barrier
        ~src_access_mask:Vkt.Access_flags.transfer_write
        ~dst_access_mask:Vkt.Access_flags.shader_read
    in
    Vkc.cmd_pipeline_barrier command_buffer
      ~src_stage_mask:Vkt.Pipeline_stage_flags.transfer
      ~dst_stage_mask:Vkt.Pipeline_stage_flags.fragment_shader
      ~image_memory_barriers:(Vkt.Image_memory_barrier.array [barrier]) ();
  | _ ->
    invalid_arg "unsupported layout transition!"

let copy_buffer ~command_pool ~width ~height t image =
  Cmd.run_one_time command_pool @@ fun command_buffer ->
  let image_subresource =
    Vkt.Image_subresource_layers.make
      ~aspect_mask:Vkt.Image_aspect_flags.color
      ~mip_level:0
      ~base_array_layer:0
      ~layer_count:1
  in
  let regions =
    Vkt.Buffer_image_copy.array [
      Vkt.Buffer_image_copy.make
        ~buffer_offset:Vkt.Device_size.zero
        ~buffer_row_length:0
        ~buffer_image_height:0
        ~image_subresource
        ~image_offset:(Vkt.Offset_3d.make ~x:0 ~y:0 ~z:0)
        ~image_extent:(Vkt.Extent_3d.make ~width ~height ~depth:1)
    ]
  in
  Vkc.cmd_copy_buffer_to_image command_buffer
    ~src_buffer:t.Buffer.buffer
    ~dst_image:image
    ~dst_image_layout:Transfer_dst_optimal
    ~regions
