(** Passing input data from the application to the GPU. *)

open Eio.Std
module Vkt = Vk.Types

type t
(** A block of memory that is written by the CPU and read by the GPU shaders. *)

val create :
  sw:Switch.t ->
  device:Vulkan.Device.t ->
  Vkt.Pipeline_layout.t * (t * t)

val set : t -> Vkt.Command_buffer.t -> int -> unit
(** [set t command_buffer frame] updates the memory for [frame] and binds it to the command buffer. *)
