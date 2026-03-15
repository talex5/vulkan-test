(** Exhaust and explosions. *)

open Eio.Std
module Vkt = Vk.Types

type t

val create :
  sw:Switch.t ->
  device:Vulkan.Device.t ->
  ubo:Ubo.t Double.t ->
  render_pass:Vkt.Render_pass.t ->
  t

val draw : t -> Double.side -> Vkt.Command_buffer.t -> unit

val add_thrust : t -> Ubo.ship -> unit
(** Add another thrust particle *)
