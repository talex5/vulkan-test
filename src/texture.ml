open Eio.Std
module Vkt = Vk.Types
module A = Vulkan.A

let blit ~src ~dst =
  Bigarray.Array1.blit src dst

let copy_to_gpu ~device ~command_pool ~dst png =
  Switch.run @@ fun sw ->
  (* Allocate GPU-visible memory on the host *)
  let w = Cairo.Image.get_width png in
  let h = Cairo.Image.get_height png in
  let size = w * h * 4 in
  let buffer =
    Vulkan.Buffer.create ~sw device size
      ~usage:Vkt.Buffer_usage_flags.transfer_src
      ~properties:Vkt.Memory_property_flags.(host_visible + host_coherent)
  in
  (* Copy the PNG data to it *)
  let mapped = Vulkan.Buffer.map ~sw buffer in
  let mapped = A.from_ptr Ctypes.(from_voidp int8_t (to_voidp (A.start mapped))) (A.length mapped) in
  let data = Ctypes.(bigarray_of_array array1) Int8_unsigned mapped in
  blit ~src:(Cairo.Image.get_data8 png) ~dst:data;
  (* Transfer host buffer to GPU-local image memory *)
  Vulkan.Image.copy_buffer ~command_pool buffer dst ~width:w ~height:h

let create ~sw ~command_pool ~device png =
  let width = Cairo.Image.get_width png in
  let height = Cairo.Image.get_height png in
  (* Allocate image memory on the GPU *)
  let image =
    Vulkan.Image.create ~sw device
      ~format:Vkt.Format.B8g8r8a8_srgb
      ~extent:(Vkt.Extent_3d.make ~width ~height ~depth:1)
      ~flags:Vkt.Image_create_flags.empty
      ~tiling:Optimal
      ~usage:Vkt.Image_usage_flags.(transfer_dst + sampled)
      ~sharing_mode:Exclusive
      ~initial_layout:Undefined
  in
  let _image_memory : Vkt.Device_memory.t = Vulkan.Image.allocate_image_memory ~sw ~device image
      ~properties:Vkt.Memory_property_flags.device_local
  in
  Vulkan.Image.transition_layout ~command_pool image
    ~old_layout:Undefined
    ~new_layout:Transfer_dst_optimal;
  copy_to_gpu ~command_pool ~device ~dst:image png;
  Vulkan.Image.transition_layout ~command_pool image
    ~old_layout:Transfer_dst_optimal
    ~new_layout:Shader_read_only_optimal;
  image
