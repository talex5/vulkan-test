(** Texture buffers in GPU memory. *)

type ('a, 'b) typ =
  | B8g8r8a8_srgb : (int32, Bigarray.int32_elt) typ
  | R8_unorm : (int, Bigarray.int8_unsigned_elt) typ
(** The type of elements in the texture. This is a subset of {!Vk.Types.Format.t}. *)

val init :
  sw:Eio.Switch.t ->
  device:Vulkan.Device.t ->
  command_pool:Vulkan.Cmd.pool ->
  ('a, 'b) typ ->
  (int * int) ->
  (('a, 'b, Bigarray.c_layout) Bigarray.Array2.t -> unit) ->
  Vk.Types.Image_view.t
(** [init ~sw ~device ~command_pool typ size fill] allocates a new image in GPU-local memory,
    initialised by calling [fill ba] on an OCaml bigarray.

    The bigarray is mapped CPU-visible memory and must not be used after this call returns
    (it gets unmapped and freed after the transfer to GPU-local memory).

    The texture has the {!Vkt.Image_usage_flags.sampled} usage and
    the layout is {!Vkt.Image_layout.Shader_read_only_optimal}. *)
