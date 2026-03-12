(** Heights and colours of the tiles. Rendered by {!Landscape}. *)

open Eio.Std

val size : int * int

val create_heights :
  sw:Switch.t ->
  command_pool:Vulkan.Cmd.pool ->
  device:Vulkan.Device.t ->
  Vk.Types.Image_view.t  
(** Allocate a texture on the GPU giving the height of each vertex on the map. *)

val create_tiles :
  sw:Switch.t ->
  command_pool:Vulkan.Cmd.pool ->
  device:Vulkan.Device.t ->
  Vk.Types.Image_view.t  
(** Allocate a texture on the GPU giving the colour of each tile on the map. *)
