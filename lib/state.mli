(** Representation of a screen of celestial bodies *)

type t
(** The abstract type representing a state *)

val init_state : Body.t list -> t
(** Lets you create a state from a list of bodies, mostly for testing currently *)

val to_json : t -> Yojson.Basic.t
(** [to_json s] constructs json representing state [s] *)

val run : float -> t -> t
(** [run t s] is the state representing [s] after [t] seconds *)

val bodies : t -> Body.t list
(** [bodies t] is the list of all bodies in the state*)

val tilt : t -> float -> t
(** [tilt t rad] is the state representing [t] tilted by [rad] degrees. The
    rotation occurs about the x-axis (if the x-axis is the one that runs
    horizontally across the center of the screen.) This method is only used for
    visual calculations *)
