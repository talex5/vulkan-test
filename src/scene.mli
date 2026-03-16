module Vkt = Vk.Types

type t

val create : sw:Eio.Switch.t -> format:Vkt.Format.t -> device:Vulkan.Device.t -> t

val draw : t -> Double.side -> Vkt.Command_buffer.t -> Surface.framebuffer -> unit

val update : t -> Surface.pointer_state -> [`Continue | `Game_over of string]

val render_pass : t -> Vkt.Render_pass.t
