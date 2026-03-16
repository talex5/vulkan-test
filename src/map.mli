(** Heights and colours of the tiles. Rendered by {!Landscape}. *)

open Eio.Std

val size : int * int

val elevation : int -> int -> float

val in_pad : float -> float -> bool
val pad_elevation : float

val tile_type : float -> float -> [`Sea | `Hill | `Lava]

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

val random_start_location : unit -> int * int
(** A random location not too near the landing pad. *)
