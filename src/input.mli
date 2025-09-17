(** Passing input data from the application to the GPU. *)

open Eio.Std
module Vkt = Vk.Types

type t
(** A block of memory that is written by the CPU and read by the GPU shaders. *)

val create :
  sw:Switch.t ->
  device:Vulkan.Device.t ->
  Vkt.Pipeline_layout.t * (t * t)

val set : t -> int -> unit
(** [set t frame] updates the memory for [frame]. *)

val bind : t -> Vkt.Command_buffer.t -> unit
(** [bind t command_buffer] binds [t] to the command buffer. *)
