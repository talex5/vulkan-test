(** The flying craft. *)

open Eio.Std
module Vkt = Vk.Types

type t

val create :
  sw:Switch.t ->
  device:Vulkan.Device.t ->
  ubo:Ubo.t Double.t ->
  render_pass:Vkt.Render_pass.t ->
  particles:Particles.t ->
  t

val draw : t -> Double.side -> Vkt.Command_buffer.t -> unit

val pos : t -> Vulkan.Vec3.t

val update : t -> Surface.pointer_state -> [`Continue | `Game_over of string]
