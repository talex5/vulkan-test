module Vkt = Vk.Types

type t = {
  render_pass : Vkt.Render_pass.t;
  ubo : Ubo.t Double.t;
  room : (Vkt.Command_buffer.t -> unit) Double.t;
  mutable frame : int;
}

val create :
  sw:Eio.Switch.t ->
  format:Vkt.Format.t ->
  device:Vulkan.Device.t ->
  Obj_format.t * Cairo.Surface.t -> t

val draw : t -> Double.side -> Vkt.Command_buffer.t -> Surface.framebuffer -> unit
