open Common

type 'a frame = {
  buffer : 'a;
  framebuffer : Vkt.Framebuffer.t;
  render_finished : Vkt.Semaphore.t;
  dma_buf_fd : Eio_unix.Fd.t;
  geometry : int * int;
}

let create_image ~device ~format (width, height) =
  Image.create device
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

type 'a t = {
  queue : 'a frame Queue.t;
  create_frame : unit -> 'a frame;
}

type dmabuf = {
  geometry : int * int;
  offset : int;
  stride : int;
  fd : Eio_unix.Fd.t;
}

let create ~sw ~device ~format ~import geometry create_framebuffer =
  let queue = Queue.create () in
  let create_frame () =
    let image = create_image ~sw ~device ~format geometry in
    let memory = Image.allocate_image_memory ~sw ~device
        ~handle_types:Vkt.External_memory_handle_type_flags.opaque_fd image
        ~properties:Vkt.Memory_property_flags.device_local
    in
    let layout = Image.get_layout ~device image in
    let framebuffer = create_framebuffer image in
    let render_finished = Semaphore.create_export ~sw device in
    let dmabuf = {
      geometry;
      offset = Vkt.Device_size.to_int (Vkt.Subresource_layout.offset layout);
      stride = Vkt.Device_size.to_int (Vkt.Subresource_layout.row_pitch layout);
      fd = Image.get_memory_fd ~sw device memory;
    } in
    let rec buffer = lazy (import ~sw ~on_release dmabuf)
    and on_release () = Queue.add (Lazy.force frame) queue
    and frame = lazy { framebuffer; buffer = Lazy.force buffer; render_finished; dma_buf_fd = dmabuf.fd; geometry } in
    Lazy.force frame
  in
  { queue; create_frame }

let get_framebuffer t =
  try Queue.take t.queue
  with Queue.Empty -> t.create_frame ()
