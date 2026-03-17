type t

val connect : OBus_connection.t -> t Lwt.t
(** [connect system_bus] connects to logind and gets the current session. *)

val take_control : ?force:bool -> t -> unit Lwt.t

val release_control : t -> unit Lwt.t

val interface : t -> Input.Interface.t
