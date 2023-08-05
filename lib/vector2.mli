(** Module for representing 2-dimensional Euclidean vectors containing floating
    point values. Supports creating and performing operations on those vectors. *)

type t = {
  x : float;
  y : float;
}
(** The type representing a vector <x, y> *)

exception ZeroVector
(** Raised when an invalid operation is used on a vector of length zero. *)

val right : t
(** The unit vector pointing right. This represents the vector <1.0, 0.0>.
    Equivalent to [make 1.0 0.0]. *)

val left : t
(** The unit vector pointing left. This represents the vector <-1.0, 0.0>.
    Equivalent to [make -1.0 0.0]. *)

val up : t
(** The unit vector pointing up. This represents the vector <0.0, 1.0>.
    Equivalent to [make 0.0 1.0]. *)

val down : t
(** The unit vector pointing down. This represents the vector <0.0, -1.0>.
    Equivalent to [make 0.0 -1.0]. *)

val zero : t
(** The zero vector. This represents the vector <0.0, 0.0>. Equivalent to
    [make 0.0, 0.0]. *)

val make : float -> float -> t
(** [make a b] returns the vector representing <a, b>. *)

val magnitude : t -> float
(** [magnitude a] is the magnitude of vector [a]. Specifically, it is the
    Euclidean length of the vector. *)

val square_magnitude : t -> float
(** [square_magnitude a] gives the squared magnitude of [a]. Equivalent to
    [magnitude a *. magnitude a] but may be more performant. *)

val compare_magnitude : t -> t -> int
(** [compare a b] is zero if [magnitude a] and [magnitude b] are equal. It is
    strictly negative if [magnitude a] is less than [magnitude b], and strictly
    positive if [magnitude a] is greater than [magnitude b]. *)

val distance : t -> t -> float
(** [distance a b] gives the Euclidean distance from [a] to [b]. *)

val normalize : t -> t
(** [normalize a] gives the unit vector in [a]'s direction. Raises: [ZeroVector]
    if [magnitude a = 0.0]. *)

val add : t -> t -> t
(** [add a b] gives the vector representing [a + b]. *)

val subtract : t -> t -> t
(** [subtract a b] gives the vector representing [a - b]. *)

val dot : t -> t -> float
(** [dot a b] gives the vector representing the dot product of [a] and [b]. *)

val scale : float -> t -> t
(** [scale c a] gives the vector representing [a] scaled by [c]. *)

val ( +< ) : t -> t -> t
(** [a +< b] is an infix alias for [add a b]. *)

val ( -< ) : t -> t -> t
(** [a -< b] is an infix alias for [subtract a b]. *)

val ( ~-< ) : t -> t
(** [~-< a] is an infix alias for [scale ~-.1. a]. *)

val ( *< ) : float -> t -> t
(** [c *< b] is an infix alias for [scale c a]. *)
