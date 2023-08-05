(** Interface for creating a window to draw state *)

type t
(** The type representing the graphics window *)

val center_vector : Vector2.t
(**A vector pointing to the center of the window*)

val make : unit -> t
(** [make ()] returns a new window *)

val run_menu : unit -> string
(** [run_menu ()] draws the menu that the user first sees *)

val draw_state_simple : State.t -> t
(** [draw_state_simple t] draws [t] on a half size window with no tilt. It does not draw the legend, info bar, control menu *)

val draw_state : State.t -> int -> float -> bool -> t
(** [draw_state s t tl w] is the window with state s tilted at tl radians and a
    legend in top left drawn. It also displays the total time t that has passed
    in the simulation *)

val close_window : t -> unit
(** [close_window w] closes the window w *)
