(** Convert strings to OCaml-style names. *)

module Dict : sig
  (** Maps strings to lists of components.

      Used to hold hard-coded special cases, e.g.
      - ["2D"] maps to [["2D"]] (not [["2";"d"]])
      - ["Vulkan12"] maps to [["vulkan";"1";"2"]].
      Also used for extension names. *)

  type t

  type word_end = string list

  val empty : t

  val add : string -> ?custom:word_end -> t -> t
  (** [add word t] extends [t] with a mapping from [word] to [[word]].
      @param custom Add a mapping to [custom] instead. *)
end

module M : Map.S with type key = string

type role = Prefix | Main | Postfix | Extension

type name = {
  prefix: string list;  (** Leading components matching the [Prefix] role *)
  main: string list;
  postfix: string list; (** Trailing components matching [Postfix] or [Extension] roles (reversed). *)
}

type dict = { words:Dict.t; roles:role M.t; context:name }

val make : dict -> string -> name
(** [make dict original] splits [original] into a list of components based on capitialisation and [dict.words],
    then splits that list according to [dict.roles], then subtracts [dict.context]. *)

val simple : string list -> name
(** [simple main] is a name with no prefix or suffix. *)

val (~:) : string -> name
(** [~: name] is [simple [name]] *)

val mu : name
(** The empty name. *)

val pp_module : name Fmt.t
val pp_var : name Fmt.t
val pp_type : name Fmt.t
val pp_constr : name Fmt.t

val remove_prefix : 'a list -> 'a list -> 'a list 
(** [remove_prefix prefix name] is [name] with matching elements from [prefix] removed. *)

val remove_context : name -> name -> name
(** Applies [remove_prefix] component-wise *)

val to_path : name -> string list
(** [to_path name] flattens [name] into a single list. *)

val ( // ) : name -> string -> name
(** [name // postfix] extends [name] with a new postfix component. *)

val is_extension : dict -> name -> bool
(** [is_extension dict name] checks whether [name.postfix] starts with an [Extension] role. *)

val full_pp : name Fmt.t
