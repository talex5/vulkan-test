open Eio.Std
module Vkt = Vk.Types
module Wl_surface = Wayland.Wayland_client.Wl_surface
module Dev_t = Drm.Dev_t

let validation = false
let validation_layers = if validation then ["VK_LAYER_KHRONOS_validation"] else []

let log fmt = Fmt.epr (fmt ^^ "@.")

type t = {
  device : Vulkan.Device.t;
  frames : Render.frame_state array;            (* Pool of framebuffers *)
  window : Window.t;
  mutable frame : int;
  frame_limit : int;
  in_flight_fence : Vkt.Fence.t;                (* Signalled when GPU finishes rendering pipeline *)

  (* Temporaries used inside draw_frame (only here to avoid recreating them all the time): *)
  image_available : Vkt.Semaphore.t;            (* Signalled when the compositer has finished showing the old image *)
}

let draw_frame t =
  let device = t.device in

  (* If we're still rendering the last frame, wait for that to finish.
     Needed because e.g. [image_available] isn't per-framebuffer. *)
  log "Wait for inFlight_fence";
  Vulkan.Fence.wait device [t.in_flight_fence];
  Vulkan.Fence.reset device [t.in_flight_fence];
  (* At this point, [image_available] is no longer in use,
     because the pipeline waited on it and reset it before finishing,
     which trigged in_flight_fence. *)

  let image_index = t.frame mod Render.n_images in
  log "Rendering frame %d with framebuffer %d" t.frame image_index;
  let frame_state = t.frames.(image_index) in

  (* Get the semaphore that the compositor's render job will signal when
     it's done reading the image: *)
  log "Import image_available";
  Vulkan.Semaphore.import t.device frame_state.dma_buf_fd t.image_available;

  (* Put commands in [command_buffer] *)
  let command_buffer = Render.record_commands frame_state ~frame:t.frame in

  (* Submit [command_buffer] to GPU *)
  log "Submit to graphicsQueue";
  Vulkan.Cmd.submit device command_buffer
    (* Wait for [image_available] before writing the pixel data *)
    ~wait:[t.image_available, Vkt.Pipeline_stage_flags.color_attachment_output]
    ~signal_semaphores:[frame_state.render_finished]
    ~fence:t.in_flight_fence;

  (* Attach [render_finished] to the dmabuf so that the compositor doesn't
     start displaying the image until the GPU has finished rendering it. *)
  log "Export render_finished";
  Vulkan.Semaphore.export t.device frame_state.render_finished frame_state.dma_buf_fd;

  (* Note: Mesa's WSI also calls set_memory_ownership when acquiring and presenting images. *)
  (* I'm not sure what that's for (it doesn't do anything on my GPU) so I skipped it. *)

  (* Tell the compositor to show the new buffer
     (the compositor will wait for render_finished, but we don't) *)
  Window.attach t.window ~buffer:frame_state.buffer

let app_info = Vulkan.Instance.application_info "vulkan-test-ocaml" ~version:(1,0,0)

let main ~net ~frame_limit =
  (* The cache spawns two threads and makes traces more confusing, so disable it. *)
  Unix.putenv "MESA_SHADER_CACHE_DISABLE" "1";
  (* show_extensions (); *)
  Switch.run @@ fun sw ->
  let instance = Vulkan.Instance.create ~sw ~validation_layers app_info in
  let transport = Wayland.Unix_transport.connect ~sw ~net () in
  let window = Window.init ~sw transport in
  let physical_device = Vulkan.Instance.find_device instance window.wayland_dmabuf.main_device in
  let device = Vulkan.Device.create ~sw physical_device in

  let format = Vkt.Format.B8g8r8a8_srgb in
  let width, height = Window.geometry window in

  (* Create pipeline and framebuffers *)
  let render_state = Render.create ~sw ~device ~format in

  let frames =
    Array.init Render.n_images (fun i ->
        log "Create framebuffer %d" i;
        let render_finished = Vulkan.Semaphore.create_export ~sw device in

        (* Create the image structure *)
        let image =
          Vulkan.Image.create ~sw device
            ~format
            ~extent:(Vkt.Extent_3d.make ~width ~height ~depth:1)
            ~usage:Vkt.Image_usage_flags.color_attachment
            ~sharing_mode:Exclusive
            ~initial_layout:Undefined
            ~flags:Vkt.Image_create_flags.alias		(* Is this needed? Mesa does this. *)
            (* Validation layer says this must be LINEAR or DRM_FORMAT_MODIFIER_EXT,
               and it doesn't like DRM_FORMAT_MODIFIER_EXT
               (requires VK_EXT_image_drm_format_modifier, which isn't available for me): *)
            ~tiling:Linear
            (* This should probably be [dma_buf_bit_ext], but then the validation layer complains
               (VUID-VkImageCreateInfo-pNext-00990). I think it's because on my card
               vkGetPhysicalDeviceImageFormatProperties2 always returns VK_ERROR_FORMAT_NOT_SUPPORTED. *)
            ~handle_types:Vkt.External_memory_handle_type_flags.opaque_fd
        in

        (* Allocate some device memory for the image *)
        let memory = Vulkan.Image.allocate_image_memory ~sw ~device image
            ~properties:Vkt.Memory_property_flags.device_local
            ~handle_types:Vkt.External_memory_handle_type_flags.opaque_fd
        in

        (* Get Linux dmabuf FD for the memory *)
        let dma_buf_fd = Vulkan.Image.get_memory_fd ~sw device memory in

        (* Get the row_pitch and offset *)
        let layout = Vulkan.Image.get_layout ~device image in

        let buffer = Vulkan.Dmabuf.create_buffer ~sw window.wayland_dmabuf
            ~on_release:ignore
            ~fd:dma_buf_fd
            ~offset:(Vkt.Device_size.to_int (Vkt.Subresource_layout.offset layout) |> Int32.of_int)
            ~stride:(Vkt.Device_size.to_int (Vkt.Subresource_layout.row_pitch layout) |> Int32.of_int)
            ~width:(Int32.of_int width)
            ~height:(Int32.of_int height)
        in

        (* Wrap the image in a framebuffer for rendering *)
        let render_pass = render_state.render_pass in
        let framebuffer = Vulkan.Image.create_framebuffer ~sw ~device ~format ~width ~height ~render_pass image in
        let extent = Vkt.Extent_2d.make ~width ~height in
        { Render.framebuffer; buffer; render_finished; dma_buf_fd; extent; ctx = render_state }
      )
  in

  (* Set up the shared state for the draw_frame callback *)
  let t = {
    device;
    frames;
    window;
    frame = 0;
    frame_limit;
    in_flight_fence = Vulkan.Fence.create ~sw device Vkt.Fence_create_flags.signaled;
    image_available = Vulkan.Semaphore.create ~sw device;
  } in
  log "Start main loop";
  while t.frame < frame_limit do
    let next_frame_due = Window.frame window in
    draw_frame t;
    Promise.await next_frame_due;
    t.frame <- t.frame + 1
  done;
  log "Frame limit reached; exiting";
  Window.destroy window

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
