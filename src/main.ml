(* A simple Vulkan/Wayland test app.
   This is intended for tracing, to help me learn about graphics.
   It is not good code to copy. For example, I generally don't bother freeing things.
   Also, it is a direct port of some C code, not idiomatic OCaml. *)

open Helpers

module Wl_surface = Wayland.Wayland_client.Wl_surface
module Wl_buffer = Wayland.Wayland_client.Wl_buffer
module Dev_t = Drm.Dev_t
open Wayland_protocols.Linux_dmabuf_unstable_v1_client

let validation = false
let validation_layers = if validation then ["VK_LAYER_KHRONOS_validation"] else []

type state = {
  device : Vkt.Device.t;
  graphics_queue : Vkt.Queue.t;
  frames : Render.frame_state array;            (* Pool of framebuffers *)
  wayland_state : Window.t;
  mutable frame : int;
  frame_limit : int;
  in_flight_fence : Vkt.Fence.t;                (* Signalled when GPU finishes rendering pipeline *)

  (* Temporaries used inside draw_frame (only here to avoid recreating them all the time): *)
  image_available_semaphore : Vkt.Semaphore.t;  (* Signalled when the compositer has finished showing the old image *)
  command_buffer : Vkt.Command_buffer.t;        (* Used to submit drawing commands *)

  (* These functions don't link statically and need looking up at runtime: *)
  import_semaphore_fd_khr : Vkt.Import_semaphore_fd_info_khr.t -> unit;
  get_semaphore_fd_khr : Vkt.Semaphore_get_fd_info_khr.t -> int;
}

(* Get active jobs on dma_buf_fd (i.e. the compositor's job) and set semaphore to be signalled when they're done. *)
let import_semaphore t dma_buf_fd semaphore =
  let int_of_unix (fd : Unix.file_descr) : int = Obj.magic fd in
  let fd = Drm.export_sync_file dma_buf_fd `RW in
  t.import_semaphore_fd_khr @@
  Vkt.Import_semaphore_fd_info_khr.make ()
    ~semaphore
    ~flags:Vkt.Semaphore_import_flags.temporary
    ~handle_type:Vkt.External_semaphore_handle_type_flags.sync_fd
    ~fd:(int_of_unix fd)         (* Takes ownership of the FD *)

(* Add semaphore's current fence to dma_buf_fd, so the compositor can wait on it. *)
let export_semaphore t fd semaphore =
  let get_fd_info = Vkt.Semaphore_get_fd_info_khr.make ()
      ~semaphore
      ~handle_type:Vkt.External_semaphore_handle_type_flags.sync_fd
  in
  let sync_file_fd = t.get_semaphore_fd_khr get_fd_info in
  let sync_file_fd : Unix.file_descr = Obj.magic (sync_file_fd : int) in
  Drm.import_sync_file fd ~sync_file_fd `RW;
  Unix.close sync_file_fd

(* Called when the Wayland compositor wants us to send the next frame *)
let rec wl_surface_frame_done t _time =
  t.frame <- t.frame + 1;
  if t.frame > t.frame_limit then (
    log "Frame limit reached; exiting";
    exit 0;
  );
  draw_frame t

and draw_frame t =
  let device = t.device in
  let surface = t.wayland_state.surface in
  (* Ask compositor to tell us when it wants the frame after this one *)
  let _cb : _ Wayland.Proxy.t = Wl_surface.frame surface (Wayland.callback (wl_surface_frame_done t)) in

  (* If we're still rendering the last frame, wait for that to finish.
     Needed because e.g. [image_available_semaphore] isn't per-framebuffer. *)
  log "Wait for inFlight_fence";
  Vkc.wait_for_fences ~device (Vkt.Fence.array [t.in_flight_fence])
    ~wait_all:true
    ~timeout:Unsigned.UInt64.max_int <?> "wait_for_fences";
  Vkc.reset_fences ~device (Vkt.Fence.array [t.in_flight_fence]) <?> "reset_fences";
  (* At this point, [image_available_semaphore] is no longer in use,
     because the pipeline waited on it and reset it before finishing,
     which trigged in_flight_fence. *)

  let image_index = t.frame mod Render.n_images in
  log "Rendering frame %d with framebuffer %d" t.frame image_index;
  let frame_state = t.frames.(image_index) in

  (* Get the semaphore that the compositor's render job will signal when
     it's done reading the image: *)
  log "Import image_available_semaphore";
  import_semaphore t frame_state.dma_buf_fd t.image_available_semaphore;

  (* Put commands in [command_buffer] *)
  Render.record_commands frame_state ~frame:t.frame t.command_buffer;

  (* Submit [command_buffer] to GPU *)
  log "Submit to graphicsQueue";
  let wait_semaphores = Vkt.Semaphore.array [t.image_available_semaphore] in
  let wait_stages = [Vkt.Pipeline_stage_flags.color_attachment_output] in
  let submit_info = Vkt.Submit_info.make ()
      (* Wait for [image_available_semaphore] before writing the pixel data *)
      ~wait_semaphores
      ~wait_dst_stage_mask:(A.of_list Vkt.Pipeline_stage_flags.ctype wait_stages)
      (* Then render the image *)
      ~command_buffers:(Vkt.Command_buffer.array [t.command_buffer])
      (* Signal [render_finished_semaphore] when finished *)
      ~signal_semaphores:(Vkt.Semaphore.array [frame_state.render_finished_semaphore])
  in
  Vkc.queue_submit t.graphics_queue ()
    ~submits:(Vkt.Submit_info.array [submit_info])
    ~fence:t.in_flight_fence <?> "queue_submit";

  (* Attach [render_finished_semaphore] to the dmabuf so that the compositor doesn't
     start displaying the image until the GPU has finished rendering it. *)
  log "Export render_finished_semaphore";
  export_semaphore t frame_state.dma_buf_fd frame_state.render_finished_semaphore;

  (* Note: Mesa's WSI also calls set_memory_ownership when acquiring and presenting images. *)
  (* I'm not sure what that's for (it doesn't do anything on my GPU) so I skipped it. *)

  (* Tell the compositor to show the new buffer
     (the compositor will wait for render_finished_semaphore, but we don't) *)
  Wl_surface.attach surface ~buffer:(Some frame_state.buffer) ~x:0l ~y:0l;
  Wl_surface.damage surface ~x:0l ~y:0l ~width:Int32.max_int ~height:Int32.max_int;
  Wl_surface.commit surface

let render dev = Dev_t.make Vkt.Physical_device_drm_properties_ext.(render_major dev, render_minor dev)
let primary dev = Dev_t.make Vkt.Physical_device_drm_properties_ext.(primary_major dev, primary_minor dev)

(* Find the graphics card that the Wayland compositor suggested we use *)
let find_wayland_device instance wayland_device =
  Log.info (fun f -> f "Wayland compositor main device is %a" Dev_t.pp wayland_device);
  let physical_devices = Vkc.enumerate_physical_devices ~instance <?> "physical device" in
  Log.info (fun f -> f "Vulkan found %d physical devices" (A.length physical_devices));
  let found = ref None in
  physical_devices |> A.iteri (fun i x ->
      let module P = Vkt.Physical_device_properties in
      let module P2 = Vkt.Physical_device_properties_2 in
      let module Drm = Vkt.Physical_device_drm_properties_ext in
      let ext_drm =
        let t = Drm.unsafe_make () in
        Ctypes.setf t Drm.Fields.s_type Vk.Types.Structure_type.Physical_device_drm_properties_ext;
        t
      in
      let properties = P.unsafe_make () in        (* Shouldn't be needed *)
      let props2 =
        P2.make ~next:(ext (Drm.addr ext_drm)) ~properties () in
      Vk.Raw.get_physical_device_properties_2 x (P2.addr props2);
      let device_name = P.device_name (P2.properties props2) in
      Log.info (fun f -> f "%d: %s" i device_name);
      Log.debug (fun f -> f "dev: %a" Vkt.Physical_device_drm_properties_ext.pp ext_drm);
      if render ext_drm = wayland_device then found := Some (i, "rendering")
      else if primary ext_drm = wayland_device then found := Some (i, "primary")
    );
  match !found with
  | None -> failwith "Wayland GPU device not found"
  | Some (i, x) ->
    Log.debug (fun f -> f "Using device %d (matches Wayland %s node)" i x);
    A.get physical_devices i

let main ~net ~frame_limit =
  (* The cache spawns two threads and makes traces more confusing, so disable it. *)
  Unix.putenv "MESA_SHADER_CACHE_DISABLE" "1";
  (* show_extensions (); *)
  let instance_extensions = [
    "VK_KHR_get_physical_device_properties2";          (* Get Unix device ID to compare with Wayland *)
    "VK_KHR_external_memory_capabilities";             (* Share images over Wayland *)
    "VK_KHR_external_semaphore_capabilities";          (* Use Linux sync files *)
  ] in
  let application_info =
    let v major minor patch = (major lsl 22) lor (minor lsl 12) lor patch in
    Vkt.Application_info.make ()
      ~application_name:"Hello Triangle"
      ~application_version:(v 1 0 0)
      ~engine_name:"No Engine"
      ~engine_version:(v 1 0 0)
      ~api_version:(v 1 2 0)
  in
  let create_info =
    Vkt.Instance_create_info.make ()
      ~application_info
      ~flags:Vkt.Instance_create_flags.empty
      ~enabled_extension_names:(string_array instance_extensions)
      ~enabled_layer_names:(string_array validation_layers)
  in
  let instance = Vkc.create_instance ~create_info () <?> "create_instance" in
  log "Create instance with %d layers" (List.length validation_layers);

  (* Initialise Wayland display and create window *)
  Switch.run @@ fun sw ->
  let transport = Wayland.Unix_transport.connect ~sw ~net () in
  let wayland_state = Window.init ~sw transport in

  (* Find the physical device that the Wayland compositor suggested *)
  let physical_device = find_wayland_device instance wayland_state.wayland_dmabuf.main_device in

  (* Create logical device with one graphics queue *)
  let graphics_family = find_graphics_family physical_device in
  let queue_create_infos =
    Vkt.Device_queue_create_info.array [
      Vkt.Device_queue_create_info.make ()
        ~queue_family_index:graphics_family
        ~queue_priorities:(float_array [1.0])
    ] in
  let device_extensions = string_array [
      (* Export image FD so we can send it to the compositor *)
      "VK_KHR_external_memory";
      "VK_KHR_external_memory_fd";
      "VK_EXT_external_memory_dma_buf";
      (* Export fences to synchronise with the compositor *)
      "VK_KHR_external_semaphore";
      "VK_KHR_external_semaphore_fd";
    ] in
  let create_info =
    Vkt.Device_create_info.make ()
      ~queue_create_infos
      ~enabled_extension_names:device_extensions
  in
  log "Create logical device";
  let device = Vk.Core.create_device () ~physical_device ~create_info <?> "create_device" in
  let graphics_queue = Vkc.get_device_queue device ~queue_family_index:graphics_family ~queue_index:0 in
  let module Device = struct let x = device end in
  let module External_memory_fd = Vk.Khr.External_memory_fd(Device) in
  let module External_semaphore = Vk.Khr.External_semaphore_fd(Device) in

  (* Create command buffer *)
  let create_info = Vkt.Command_pool_create_info.make ()
      ~flags:Vkt.Command_pool_create_flags.reset_command_buffer
      ~queue_family_index:graphics_family
  in
  log "Create command pool";
  let command_pool = Vkc.create_command_pool () ~device ~create_info <?> "create_command_pool" in

  let allocate_info = Vkt.Command_buffer_allocate_info.make ()
      ~command_pool
      ~level:Primary
      ~command_buffer_count:1
  in
  let command_buffer =
    match Vkc.allocate_command_buffers ~device ~allocate_info <?> "allocate_command_buffers" |> A.to_list with
    | [x] -> x
    | _ -> failwith "Wrong number of command buffers!"
  in

  (* Properties for creating framebuffers *)
  let drm_format = wayland_state.wayland_dmabuf.drm_format in
  assert (drm_format.code = Drm.Format.Code.xr24);
  let format = Vkt.Format.B8g8r8a8_srgb in
  let width, height = 640, 480 in
  let view_formats = A.of_list Vkt.Format.ctype [ format ] in
  let ext2 = Vkt.Image_format_list_create_info.make ~view_formats () in
  let ext1 = Vkt.External_memory_image_create_info.make ()
      (* This should probably be [dma_buf_bit_ext], but then the validation layer complains
         (VUID-VkImageCreateInfo-pNext-00990). I think it's because on my card
         vkGetPhysicalDeviceImageFormatProperties2 always returns VK_ERROR_FORMAT_NOT_SUPPORTED. *)
      ~handle_types:Vkt.External_memory_handle_type_flags.opaque_fd
      ~next:(ext (Vkt.Image_format_list_create_info.addr ext2))
  in
  let img_info = Vkt.Image_create_info.make ()
      ~next:(ext (Vkt.External_memory_image_create_info.addr ext1))
      ~flags:Vkt.Image_create_flags.alias               (* Is this needed? Mesa does this. *)
      ~image_type:N2d
      ~mip_levels:1
      ~array_layers:1
      ~format
      ~extent:(Vkt.Extent_3d.make ~width ~height ~depth:1)
      ~samples:Vkt.Sample_count_flags.n1
      (* Validation layer says this must be LINEAR or DRM_FORMAT_MODIFIER_EXT,
         and it doesn't like DRM_FORMAT_MODIFIER_EXT
         (requires VK_EXT_image_drm_format_modifier, which isn't available for me): *)
      ~tiling:Linear
      ~usage:Vkt.Image_usage_flags.color_attachment
      ~sharing_mode:Exclusive
      ~initial_layout:Undefined
  in

  (* Create pipeline and framebuffers *)
  let render_state, inputs = Render.create ~physical_device ~device ~format in

  let memory_properties = Vkc.get_physical_device_memory_properties physical_device in

  let frames =
    inputs |> Array.mapi (fun i (uniform_buffer_mapped, descriptor_set) ->
        log "Create framebuffer %d" i;
        let render_finished_semaphore = create_export_semaphore device in

        (* Create the image structure *)
        log "vkCreateImage";
        let image = Vkc.create_image ~device ~create_info:img_info () <?> "create_image" in

        (* Allocate some device memory for the image *)
        let reqs = Vkc.get_image_memory_requirements ~device image in

        let memory_export_info =
          Vkt.Export_memory_allocate_info.make ()
            ~handle_types:Vkt.External_memory_handle_type_flags.opaque_fd
        in
        let memory_dedicated_info = Vkt.Memory_dedicated_allocate_info.make ()
            ~next:(ext (Vkt.Export_memory_allocate_info.addr memory_export_info))
            ~image
        in
        let allocate_info =
          Vkt.Memory_allocate_info.make ()
            ~next:(ext (Vkt.Memory_dedicated_allocate_info.addr memory_dedicated_info))
            ~allocation_size:(Vkt.Memory_requirements.size reqs)
            ~memory_type_index:(
              find_memory_type
                ~memory_properties
                ~type_filter:(Vkt.Memory_requirements.memory_type_bits reqs)
                ~properties:Vkt.Memory_property_flags.device_local
            )
        in
        log "vkAllocateMemory";
        let memory = Vkc.allocate_memory ~device ~allocate_info () <?> "allocate_memory" in

        (* Attach the memory to the image *)
        log "vkBindImageMemory";
        Vkc.bind_image_memory ~device image memory ~memory_offset:Vkt.Device_size.zero <?> "bind_image_memory";

        (* Get Linux dmabuf FD for the memory *)
        log "vkGetMemoryFdKHR";
        let get_fd_info = Vkt.Memory_get_fd_info_khr.make ()
            ~memory
            ~handle_type:Vkt.External_memory_handle_type_flags.opaque_fd
        in
        let dma_buf_fd =
          let x : int = External_memory_fd.get_memory_fd_khr ~get_fd_info <?> "get_memory_fd_khr" in
          (Obj.magic x : Unix.file_descr)
        in
        (* Get the row_pitch and offset *)
        let subresource = Vkt.Image_subresource.make
            ~aspect_mask:Vkt.Image_aspect_flags.color
            ~mip_level:0
            ~array_layer:0
        in
        let image_layout = Vkc.get_image_subresource_layout ~device image subresource in

        (* Register the dmabuf with the Wayland compositor *)
        let params = Zwp_linux_dmabuf_v1.create_params wayland_state.wayland_dmabuf.linux_dmabuf @@ object
            inherit [_] Zwp_linux_buffer_params_v1.v1
            method on_created _ = assert false        (* Not used for immediate creation *)
            method on_failed _ =
              (* The spec says sending this is optional, so it's a bit pointless. *)
              Log.warn (fun f -> f "create_buffer failed")
          end
        in
        let modifier = drm_format.modifier in
        Zwp_linux_buffer_params_v1.add params
          ~fd:dma_buf_fd
          ~plane_idx:0l
          ~offset:(Vkt.Device_size.to_int (Vkt.Subresource_layout.offset image_layout) |> Int32.of_int)
          ~stride:(Vkt.Device_size.to_int (Vkt.Subresource_layout.row_pitch image_layout) |> Int32.of_int)
          ~modifier_hi:(Int64.shift_right_logical modifier 32 |> Int64.to_int32)
          ~modifier_lo:(Int64.to_int32 modifier);
        let buffer = Zwp_linux_buffer_params_v1.create_immed params
            ~width:(Int32.of_int width)
            ~height:(Int32.of_int height)
            ~format:drm_format.code
            ~flags:0l
              object
                inherit [_] Wl_buffer.v1
                method on_release _ = ()
              end
        in
        Zwp_linux_buffer_params_v1.destroy params;

        (* Wrap the image in a framebuffer for rendering *)
        let view =
          let components = Vkt.Component_mapping.make ~r:Identity ~g:Identity ~b:Identity ~a:Identity in
          let subresource_range = Vkt.Image_subresource_range.make
              ~aspect_mask:Vkt.Image_aspect_flags.color
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
          Vkc.create_image_view ~device ~create_info () <?> "create_image_view"
        in
        let attachments = Vkt.Image_view.array [view] in
        let framebuffer_info = Vkt.Framebuffer_create_info.make ()
            ~render_pass:render_state.render_pass
            ~attachments
            ~width
            ~height
            ~layers:1
        in
        let framebuffer = Vkc.create_framebuffer ~device ~create_info:framebuffer_info () <?> "create_framebuffer" in
        let extent = Vkt.Extent_2d.make ~width ~height in
        { Render.framebuffer; buffer; render_finished_semaphore; dma_buf_fd; uniform_buffer_mapped; descriptor_set; extent; ctx = render_state }
      )
  in
  (* Set up the shared state for the draw_frame callback *)
  let create_info = Vkt.Semaphore_create_info.make () in
  let image_available_semaphore = Vkc.create_semaphore ~device ~create_info () <?> "create_semaphore" in

  let create_info = Vkt.Fence_create_info.make ~flags:Vkt.Fence_create_flags.signaled () in
  let in_flight_fence = Vkc.create_fence ~device ~create_info () <?> "create_fence" in

  let import_semaphore_fd_khr x =
    External_semaphore.import_semaphore_fd_khr ~import_semaphore_fd_info:x <?> "vkImportSemaphoreFdKHR"
  in
  let get_semaphore_fd_khr x = External_semaphore.get_semaphore_fd_khr ~get_fd_info:x <?> "get_semaphore_fd_khr" in
  let state = {
    device;
    graphics_queue;
    command_buffer;
    frames;
    wayland_state;
    frame = 0;
    frame_limit;
    in_flight_fence;
    image_available_semaphore;
    import_semaphore_fd_khr;
    get_semaphore_fd_khr;
  } in
  log "Start main loop";
  draw_frame state;
  Fiber.await_cancel ()

let () =
  (* Configure logging *)
  Logs.set_level (Some Warning);
  Logs.set_reporter (Logs_fmt.reporter ~pp_header:Fmt.nop ());
  (* Start event loop *)
  Eio_main.run @@ fun env ->
  (* Parse command-line arguments *)
  let frame_limit =
    match Sys.argv with
    | [| _; x |] -> int_of_string x
    | _ -> 200
  in
  try
    main ~net:env#net ~frame_limit
  with Window.Closed ->
    log "Window closed; exiting"
