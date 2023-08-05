(** A mutable module for allowing a user to create a state through the command
   line *)

type t
(** The state representing a current cli session *)

val poll_state : t -> State.t
(** The current state being created *)

val session : t
(** Represents a new session *)

val start_session : t -> State.t
(** Creates a new command line session for creating a state *)
