open Eio.Std

type t

val allocate : sw:Switch.t -> device:Vulkan.Device.t -> Obj_format.t -> t
(** Copy vertex data to the CPU. *)

val record : t -> Vulkan.Cmd.t -> unit
(** Record operations to draw the model. *)
