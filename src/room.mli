(** Render the room. *)

open Eio.Std
module Vkt = Vk.Types

val create :
  sw:Switch.t ->
  device:Vulkan.Device.t ->
  ubo:Ubo.t Double.t ->
  render_pass:Vkt.Render_pass.t ->
  Obj_format.t * Cairo.Surface.t ->
  (Vkt.Command_buffer.t -> unit) Double.t
