module Vkt = Vk.Types

type t = {
  render_pass : Vkt.Render_pass.t;
  ubo : Ubo.t Double.t;
  landscape : Landscape.t;
  ship : Ship.t;
}

val create : sw:Eio.Switch.t -> format:Vkt.Format.t -> device:Vulkan.Device.t -> t

val draw : t -> Double.side -> Vkt.Command_buffer.t -> Surface.framebuffer -> unit
