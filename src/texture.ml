open Eio.Std
module Vkt = Vk.Types
module A = Vulkan.A

type ('a, 'b) typ =
  | B8g8r8a8_srgb : (int32, Bigarray.int32_elt) typ
  | R8_unorm : (int, Bigarray.int8_unsigned_elt) typ

let ctype (type a b) : (a, b) typ -> a Ctypes.typ = function
  | B8g8r8a8_srgb -> Ctypes.int32_t
  | R8_unorm -> Ctypes.int8_t

let ba_kind (type a b) : (a, b) typ -> (a, b) Bigarray.kind = function
  | B8g8r8a8_srgb -> Int32
  | R8_unorm -> Int8_unsigned

let map ~sw ~typ (width, height) buffer =
  let kind = ba_kind typ in
  let typ = ctype typ in
  let mapped = Vulkan.Buffer.map ~sw buffer in
  let mapped = A.from_ptr Ctypes.(from_voidp typ (to_voidp (A.start mapped))) (width * height) in
  let data = Ctypes.(bigarray_of_array array1) kind mapped in
  Bigarray.reshape_2 (Bigarray.genarray_of_array1 data) width height

let write_to_gpu ~device ~command_pool ~gpu_image ~typ (width, height) fill =
  Switch.run @@ fun sw ->
  (* Allocate GPU-visible memory on the host *)
  let size = width * height * (Bigarray.kind_size_in_bytes (ba_kind typ)) in
  let buffer =
    Vulkan.Buffer.create ~sw device size
      ~usage:Vkt.Buffer_usage_flags.transfer_src
      ~properties:Vkt.Memory_property_flags.(host_visible + host_coherent)
  in
  (* Copy the data to it *)
  fill (map ~sw ~typ (width, height) buffer);
  (* Transfer host buffer to GPU-local image memory *)
  Vulkan.Image.copy_buffer ~command_pool ~width ~height buffer gpu_image

let init (type a b) ~sw ~device ~command_pool typ (width, height) fill =
  let format : Vkt.Format.t = match (typ : (a, b) typ) with
    | B8g8r8a8_srgb -> B8g8r8a8_srgb
    | R8_unorm -> R8_unorm
  in
  let gpu_image =
    Vulkan.Image.create ~sw device
      ~format
      ~extent:(Vkt.Extent_3d.make ~width ~height ~depth:1)
      ~flags:Vkt.Image_create_flags.empty
      ~tiling:Optimal
      ~usage:Vkt.Image_usage_flags.(transfer_dst + sampled)
      ~sharing_mode:Exclusive
      ~initial_layout:Undefined
  in
  let _image_memory : Vkt.Device_memory.t = Vulkan.Image.allocate_image_memory ~sw ~device gpu_image
      ~properties:Vkt.Memory_property_flags.device_local
  in
  Vulkan.Image.transition_layout ~command_pool gpu_image
    ~old_layout:Undefined
    ~new_layout:Transfer_dst_optimal;
  write_to_gpu ~command_pool ~device ~gpu_image ~typ (width, height) fill;
  Vulkan.Image.transition_layout ~command_pool gpu_image
    ~old_layout:Transfer_dst_optimal
    ~new_layout:Shader_read_only_optimal;
  Vulkan.Image.create_view ~sw ~format ~device ~aspect_mask:Vkt.Image_aspect_flags.color gpu_image
