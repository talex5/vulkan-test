type t

val pp : t Fmt.t

val write : t -> float Ctypes.CArray.t -> unit
(** [write t c] copies [t] to C structure [c], which must be an array of 16 floats. *)

val id : t
(** The identity matrix. *)

val ( * ) : t -> t -> t
(** Matrix multiplication. *)

val ( + ) : t -> t -> t
(** Matrix addition. *)

val translate : Vec3.t -> t
(** [translate v] moves all points by adding the vector [v]. *)

val rot_x : float -> t
(** [rot_x r] rotates [r] radians around the x axis. *)

val rot_y : float -> t
(** [rot_y r] rotates [r] radians around the y axis. *)

val rot_z : float -> t
(** [rot_z r] rotates [r] radians around the z axis. *)

val scale : float -> t
(** [scale f] scales all axes by a factor of [f]. *)

val scale_x : float -> t
val scale_y : float -> t
val scale_z : float -> t

val perspective_projection : fov_y:float -> aspect:float -> z_near:float -> z_far:float -> t
(** [perspective_projection ~fov_y ~aspect ~z_near ~z_far] is a 3D projection matrix.

    See {{: https://www.mauriciopoppe.com/notes/computer-graphics/viewing/projection-transform/}
    Transformation Matrix for Projection of 3D Objects into a 2D Plane}.

    @param fov_y : vertical field of view (radians)
    @param aspect : aspect ratio (screen width divided by height)
    @param z_near : distance to near clipping plane (i.e. the distance from your eye to the screen)
    @param z_far : distance to far clipping plane *)
