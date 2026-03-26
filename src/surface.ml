module Vkt = Vk.Types

(* Default image creation, without modifiers or external memory. *)
let create_image ~sw ~device ~format ~drm_format (width, height) =
  let image =
    Vulkan.Image.create device ~sw
    ~format
    ~extent:(Vkt.Extent_3d.make ~width ~height ~depth:1)
    ~usage:Vkt.Image_usage_flags.color_attachment
    ~sharing_mode:Exclusive
    ~initial_layout:Undefined
    ~flags:Vkt.Image_create_flags.alias		(* Is this needed? Mesa does this. *)
    ~tiling:Linear
    ~handle_types:Vkt.External_memory_handle_type_flags.opaque_fd
  in
  let memory = Vulkan.Image.allocate_image_memory ~sw ~device image
      ~handle_types:Vkt.External_memory_handle_type_flags.opaque_fd
      ~properties:Vkt.Memory_property_flags.device_local
  in
  let layout = Vulkan.Image.get_layout ~device image in
  let dmabuf = { Vulkan.Swap_chain.
    geometry = (width, height);
    offset = Vkt.Device_size.to_int (Vkt.Subresource_layout.offset layout);
    stride = Vkt.Device_size.to_int (Vkt.Subresource_layout.row_pitch layout);
    fd = Vulkan.Image.get_memory_fd ~sw device memory;
    drm_format;
  } in
  image, dmabuf

type buffer = <
  attach : unit;
  dma_buf_fd : Eio_unix.Fd.t;
>

type framebuffer = {
  buffer : buffer;
  framebuffer : Vkt.Framebuffer.t;
  render_finished : Vkt.Semaphore.t;
  geometry : int * int;
}

type t = <
  format : Vkt.Format.t;
  geometry : int * int;

  create_image :
    sw:Eio.Switch.t ->
    device:Vulkan.Device.t ->
    on_release:(unit -> unit) ->
    (int * int) ->
    Vkt.Image.t * buffer;

  frame : unit Eio.Promise.t;
  vulkan_extensions : string list;
>
