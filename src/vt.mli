(** Render images directly to a display device, for use with Linux virtual terminals. *)

open Eio.Std

type t
(** A physical display screen. *)

val init : sw:Switch.t -> unit -> t
(** [init ~sw ()] finds a display device and chooses a connected connector on it.

    When [sw] is turned off, the display is reset to its initial state.

    Note: assumes connector connectors have already been assigned to a CRTC
    and configured with a mode. *)

val device : t -> Drm.Dev_t.t
(** [device t] is the ID of the chosen device. *)

val surface : t -> Surface.t
(** [surface t] is the rendering target for [t]. *)
