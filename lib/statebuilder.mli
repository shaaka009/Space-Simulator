(** A module for interactively creating a state. *)

type t
(** The abstract type representing a state builder. *)

val empty : t
(** [empty] is the state builder representing a state with no bodies *)

val to_state : t -> State.t
(** [to_state s] is the state represented by state builder [s]. Any bodies with
    non-positive mass or radius will be updated to have [0.01] for their
    corresponding non positve fields. Bodies with duplicate names will be
    removed and a warning message will be printed. *)

val to_json : t -> Yojson.Basic.t
(** [to_json s] is the json representation of [to_state s]. *)

val from_json : Yojson.Basic.t -> t
(** [from_json s] is the statebuilder representing json [s] *)

val register_click : Vector2.t -> t -> Body.t option
(** [register_click v s] returns [Some b] where [b] is a body overlapping
    position [v] in state builder [s]. Returns [None] if no body overlaps [v] in
    [s]. *)

val get_body : string -> t -> Body.t option
(** [get_body n] returns an option containing the body named n if it exists, or
    [None] if it does not exist *)

val add_body : Body.t -> t -> t
(** [add_body b s] returns a state builder that is the same as state builder [s]
    but with new body [b]. *)

val delete_body : Body.t -> t -> t
(** [delete_body b s] returns a state builder that is the same as state builder
    [s] but without body [b]. This is equivalent to [s] if [b] is not a body in
    [s]. *)

val set_color : Body.t -> int -> int -> int -> t -> t
(** [set_color body r g b s] returns a state builder that is the same as state
    builder [s] but with [body] updated to have the RGB color [r], [g], [b]. *)

val set_radius : Body.t -> float -> t -> t
(** [set_radius body r s] returns a state builder that is the same as state
    builder [s] but with [body] updated to have the radius [r]. *)

val set_mass : Body.t -> float -> t -> t
(** [set_mass body m s] returns a state builder that is the same as state
    builder [s] but with [body] updated to have the mass [m]. *)

val set_position : Body.t -> Vector2.t -> t -> t
(** [set_mass body p s] returns a state builder that is the same as state
    builder [s] but with [body] updated to have the position [p]. *)

val set_velocity : Body.t -> Vector2.t -> t -> t
(** [set_mass body p s] returns a state builder that is the same as state
    builder [s] but with [body] updated to have the velocity [v]. *)

val set_name : Body.t -> string -> t -> t
(** [set_name body name s] returns a state builder that is the same as state
    builder [s] but with [body] updated to have the name [name] *)
