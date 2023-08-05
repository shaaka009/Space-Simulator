open Yojson.Basic.Util

type t = Body.t list

let empty : t = []

let duplicate_warn name =
  "\n" ^ {|WARNING: Multiple bodies had the name "|} ^ name
  ^ {|". Body names must be unique. Only one body will be kept.|}

let rec register_click (v : Vector2.t) (s : t) : Body.t option =
  match s with
  | [] -> None
  | body :: t -> if in_bounds v body then Some body else register_click v t

and in_bounds (click : Vector2.t) (body : Body.t) : bool =
  Vector2.distance click (Body.position body) <= Body.radius body

let add_body (body : Body.t) (s : t) : t = body :: s

let get_body (name : string) (s : t) : Body.t option =
  List.find_opt (fun x -> Body.name x = name) s

let delete_body (body : Body.t) (s : t) : t = List.filter (fun x -> x <> body) s

let unpack_body (body : Body.t) =
  Body.
    ( radius body,
      mass body,
      position body,
      velocity body,
      red body,
      green body,
      blue body,
      name body )

let rec set_color (body : Body.t) (r : int) (g : int) (b : int) (s : t) : t =
  List.map (fun x -> if x = body then update_color x r g b else x) s

and update_color (body : Body.t) (rd : int) (gn : int) (bl : int) : Body.t =
  let r, m, p, v, _, _, _, n = unpack_body body in
  Body.(make r m p v rd gn bl n)

let rec set_radius (body : Body.t) (r : float) (s : t) : t =
  List.map (fun x -> if x = body then update_radius x r else x) s

and update_radius (body : Body.t) (r : float) : Body.t =
  let _, m, p, v, rd, gn, bl, n = unpack_body body in
  Body.(make r m p v rd gn bl n)

let rec set_mass (body : Body.t) (m : float) (s : t) : t =
  List.map (fun x -> if x = body then update_mass x m else x) s

and update_mass (body : Body.t) (m : float) : Body.t =
  let r, _, p, v, rd, gn, bl, n = unpack_body body in
  Body.(make r m p v rd gn bl n)

let set_position (body : Body.t) (p : Vector2.t) (s : t) : t =
  List.map (fun x -> if x = body then Body.move_to p x else x) s

let set_velocity (body : Body.t) (v : Vector2.t) (s : t) : t =
  List.map (fun x -> if x = body then Body.set_velocity v x else x) s

let rec set_name (body : Body.t) (n : string) (s : t) : t =
  List.map (fun x -> if x = body then update_name x n else x) s

and update_name (body : Body.t) (n : string) : Body.t =
  let r, m, p, v, rd, gn, bl, _ = unpack_body body in
  Body.(make r m p v rd gn bl n)

(* [correct_body b] is the body b modified to ensure it has positive mass and
   radius. *)
let rec correct_body (body : Body.t) : Body.t =
  if Body.mass body <= 0.0 then correct_body (update_mass body 0.01)
  else if Body.radius body <= 0.0 then correct_body (update_radius body 0.01)
  else body

let rec unique_names (lst : Body.t list) : Body.t list =
  match lst with
  | [] -> []
  | h :: t ->
      if List.exists (fun x -> Body.name x = Body.name h) t then
        print_endline (duplicate_warn (Body.name h))
      else ();
      h :: List.filter (fun x -> Body.name x <> Body.name h) (unique_names t)

let to_state (s : t) : State.t =
  State.init_state (s |> List.map correct_body |> unique_names)

let to_json (s : t) : Yojson.Basic.t = s |> to_state |> State.to_json

let bodies_w_color_from_json j =
  let radius = j |> member "radius" |> to_float in
  let mass = j |> member "mass" |> to_float in

  let position_x = j |> member "position_x" |> to_float in
  let position_y = j |> member "position_y" |> to_float in
  let position = Vector2.make position_x position_y in

  let velocity_x = j |> member "velocity_x" |> to_float in
  let velocity_y = j |> member "velocity_y" |> to_float in
  let velocity = Vector2.make velocity_x velocity_y in

  let red = j |> member "red" |> to_int in
  let green = j |> member "green" |> to_int in
  let blue = j |> member "blue" |> to_int in

  let name = j |> member "name" |> to_string in
  Body.make radius mass position velocity red green blue name

let from_json (json : Yojson.Basic.t) : t =
  json |> member "bodies" |> to_list
  |> List.map bodies_w_color_from_json
  |> List.fold_left (fun x y -> add_body y x) empty
