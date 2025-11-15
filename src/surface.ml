module Vkt = Vk.Types

let create_image ~device ~format (width, height) =
  Vulkan.Image.create device
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

type buffer = < attach : unit >

type framebuffer = {
  buffer : buffer;
  framebuffer : Vkt.Framebuffer.t;
  render_finished : Vkt.Semaphore.t;
  dma_buf_fd : Eio_unix.Fd.t;
  geometry : int * int;
}

type t = <
  format : Vkt.Format.t;
  geometry : int * int;
  create_image : sw:Eio.Switch.t -> device:Vulkan.Device.t -> (int * int) -> Vkt.Image.t;
  import_buffer : sw:Eio.Switch.t -> on_release:(unit -> unit) -> Vulkan.Swap_chain.dmabuf -> buffer;
  frame : unit Eio.Promise.t;
>
