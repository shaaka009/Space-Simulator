(** Representation of a celestial body. *)

type t
(** The abstract type of values representing celestial bodies *)

exception ZeroMass
(** Raised when an invalid operation called on body with no mass *)

val make :
  float -> float -> Vector2.t -> Vector2.t -> int -> int -> int -> string -> t
(** [make r m pos vel rd gn bl n] creates a celestial body with radius [r], mass
    [m], position [p], velocity [vel], color [rd, gn, bl], and name [n]. *)

val radius : t -> float
(** [radius a] is the radius of celestial body [a] in meters*)

val mass : t -> float
(** [mass a] is the mass of celestial body [a] in kilograms*)

val position : t -> Vector2.t
(** [position a] is the position of celestial body [a] in meters*)

val z_pos : t -> float
(** [ z_pos a] returns the display z coordinate of the body [a]*)

val velocity : t -> Vector2.t
(** [velocity a] is the velocity of celestial body [a] in meters/second *)

val name : t -> string
(** [name a] is the name of celestial body [a]. *)

val set_name : string -> t -> t
(** [set_name s b] is the body [b] but with name [s]. *)

val red : t -> int
(** [red a] is the red component of the color of t*)

val green : t -> int
(** [red a] is the red component of the color of t*)

val blue : t -> int
(** [red a] is the red component of the color of t*)

val distance : t -> t -> float
(** [distance a b] is the distance from [a] to [b] in meters*)

val move_to : Vector2.t -> t -> t
(** [move_to pos a] is [a] with position [pos] *)

val set_z : t -> float -> t
(** [set_z a flt] is [a] with z_position [flt] *)

val set_velocity : Vector2.t -> t -> t
(** [set_velocity vel a] is [a] with velocity [vel] *)

val move_for_time : float -> t -> t
(** [move_for_time t a] is the celestial body [a] after moving for [t] time at
    its velocity *)

val force_for_time : Vector2.t -> float -> t -> t
(** [apply_force f t a] is the celesital body resulting from applying [f] in
    Newtons to [a] for [t]. Raises [ZeroMass] iff [mass a] is zero. Note: Does
    not change position, only velocity.*)

val collides : t -> t -> bool
(** [collides a b] is true only if [a] overlaps [b] and [distance a b] is not
    [0.0] *)

val colliders : t list -> (t * t) list
(** [collides t] is a list of pairs of all celestial bodies that collide with
    each other in t. *)

val separate : t * t -> t * t
(** [separate (a, b)] are new celestial bodies [(c, d)] representing [(a, b)]
    moved apart. They will each be moved exactly half the intersection length. *)

val elastic_separation : t * t -> t * t
(** [elastic_separation (a, b)] is [separate (a, b)] where the new bodies have
    their momentums calculated according to elastic collisions. Raises
    [ZeroMass] iff [mass a +. mass b = 0.] and [collides a b] *)

val inelastic_separation : t * t -> t * t
(** [inelastic_separation (a, b)] is [separate (a, b)] where the new bodies have
    their momentums calculated according to inelastic collisions. Raises
    [ZeroMass] iff [mass a +. mass b = 0.] and [collides a b]*)

val gravity_for_time : t list -> t -> float -> t
(** [gravity_for_time l b t] is the celestial body resulting from applying the
    force of gravity from all bodies in list [l] to body [b] for [t] seconds.
    Note: Changes both velocity and position of bodies*)
