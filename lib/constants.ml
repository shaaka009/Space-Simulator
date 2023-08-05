(** Collection of constants for use in multiple files *)

(** The gravitational constant G *)
let gravitational_constant = 6.67408e-11

(** Tells the drawing tool whether we want to allow the user to tilt or not *)
let tilt_implement = true

(** [scene_path s] returns the location of the scene with name s. Requires that
    s does not have the .json extension already *)
let scene_path scene = "scenes" ^ Filename.dir_sep ^ scene ^ ".json"

(** [scene_path_2 s] returns the location of the scene with name s. Requires
    that s does have the .json extension already *)
let scene_path_2 scene = "scenes" ^ Filename.dir_sep ^ scene

(** A default scene for the drawing tool *)
let default_scene = scene_path "bounce"

(** Lets the State calculator know whether to implement elastic collisions or
    ineleastic collisions instead *)
let elastic_collisions = true

(** Default mass for the CLIBuilder *)
let default_mass = 1.9e15

(** Default radius for the CLIBuilder *)
let default_radius = 50.

(** Default x position for the CLIBuilder *)
let default_pos_x = 600.

(** Default y position for the CLIBuilder *)
let default_pos_y = 400.

(** Default x velocity for the CLIBuilder *)
let default_vel_x = 0.

(** Default y position for the CLIBuilder *)
let default_vel_y = 0.

(** Default red color value for the CLIBuilder *)
let default_red = 100

(** Default green color value for the CLIBuilder *)
let default_green = 100

(** Default blue color value for the CLIBuilder *)
let default_blue = 0
