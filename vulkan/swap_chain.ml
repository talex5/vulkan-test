open Common

type frame = {
  wl_buffer : Dmabuf.buffer;
  framebuffer : Vkt.Framebuffer.t;
  render_finished : Vkt.Semaphore.t;
  dma_buf_fd : Eio_unix.Fd.t;
}

let create_image ~device ~extent format =
  Image.create device
    ~format
    ~extent
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

type t = {
  queue : frame Queue.t;
  create_frame : unit -> frame;
}

let create ~sw ~dmabuf ~device ~format (width, height) create_framebuffer =
  let queue = Queue.create () in
  let create_frame () =
    let extent = Vkt.Extent_3d.make ~width ~height ~depth:1 in
    let image = create_image ~sw ~device ~extent format in
    let memory = Image.allocate_image_memory ~sw ~device
        ~handle_types:Vkt.External_memory_handle_type_flags.opaque_fd image
        ~properties:Vkt.Memory_property_flags.device_local
    in
    let layout = Image.get_layout ~device image in
    let dma_buf_fd = Image.get_memory_fd ~sw device memory in
    let framebuffer = create_framebuffer (width, height) image in
    let render_finished = Semaphore.create_export ~sw device in
    let frame wl_buffer = { framebuffer; wl_buffer; render_finished; dma_buf_fd } in
    frame @@ Dmabuf.create_buffer ~sw dmabuf
      ~on_release:(fun wl_buffer -> Queue.add (frame wl_buffer) queue)
      ~fd:dma_buf_fd
      ~offset:(Vkt.Device_size.to_int (Vkt.Subresource_layout.offset layout) |> Int32.of_int)
      ~stride:(Vkt.Device_size.to_int (Vkt.Subresource_layout.row_pitch layout) |> Int32.of_int)
      ~width:(Int32.of_int width)
      ~height:(Int32.of_int height)
  in
  { queue; create_frame }

let get_framebuffer t =
  try Queue.take t.queue
  with Queue.Empty -> t.create_frame ()
