type t = Statebuilder.t

let poll_state = Statebuilder.to_state

let body_string b =
  let name = Body.name b in
  let radius = Body.radius b in
  let mass = Body.mass b in
  let position = Body.position b in
  let velocity = Body.velocity b in
  let rd = Body.red b in
  let gn = Body.green b in
  let bl = Body.blue b in
  "Name: " ^ name ^ "\n" ^ "\t Radius: " ^ string_of_float radius ^ "\n"
  ^ "\t Mass: " ^ string_of_float mass ^ "\n" ^ "\t Position: <"
  ^ string_of_float position.x ^ ", " ^ string_of_float position.y ^ ">\n"
  ^ "\t Velocity: <" ^ string_of_float velocity.x ^ ", "
  ^ string_of_float velocity.y ^ ">\n" ^ "\t RGB Color: (" ^ string_of_int rd
  ^ ", " ^ string_of_int gn ^ ", " ^ string_of_int bl ^ ")"

let state_string a = String.concat "\n" (List.map body_string (State.bodies a))

let rec float_prompt (prompt : string) (positive : bool) : float =
  print_endline prompt;
  match Float.of_string_opt (input_line stdin) with
  | None ->
      print_endline
        "Invalid float entered. Examples of valid floats are '1.0' or '10.5'";
      float_prompt prompt positive
  | Some f when f <= 0. && positive ->
      print_endline "Invalid float entered. Floats must be greater than 0.";
      float_prompt prompt positive
  | Some f -> f

let rec int_prompt (prompt : string) (min : int) (max : int) : int =
  print_endline prompt;
  match int_of_string_opt (input_line stdin) with
  | None ->
      print_endline
        "Invalid int entered. Examples of valid ints are '1' or '200'";
      int_prompt prompt min max
  | Some n when min <= n && n <= max -> n
  | Some _ ->
      print_endline
        ("Enter an int between " ^ string_of_int min ^ " and "
       ^ string_of_int max ^ " inclusive.");
      int_prompt prompt min max

let vec_prompt (prompt : string) : Vector2.t =
  print_endline prompt;
  let x = float_prompt "Enter the x value as a float:" false in
  let y = float_prompt "Enter the y value as a float:" false in
  Vector2.make x y

let string_prompt (prompt : string) : string =
  print_endline prompt;
  input_line stdin

let color_prompt (prompt : string) : int * int * int =
  print_endline prompt;
  let r = int_prompt "Enter an integer for red between 0 and 255" 0 255 in
  let g = int_prompt "Enter an integer for green between 0 and 255" 0 255 in
  let b = int_prompt "Enter an integer for blue between 0 and 255" 0 255 in
  (r, g, b)

let rec cli (builder : t) : State.t =
  let _ = Window.draw_state_simple (Statebuilder.to_state builder) in
  print_endline (state_string (Statebuilder.to_state builder));
  print_endline
    "Enter command. Type 'Help' (without single quotes) for a list of commands.";
  match String.lowercase_ascii (input_line stdin) with
  | "help" -> help builder
  | "add body" -> add_body builder
  | "remove body" -> remove_body builder
  | "set position" -> set_position builder
  | "set color" -> set_color builder
  | "set velocity" -> set_velocity builder
  | "set radius" -> set_radius builder
  | "set mass" -> set_mass builder
  | "save scene" -> save_scene builder
  | "quit" -> Statebuilder.to_state builder
  | _ ->
      print_endline
        "Invalid command. Type 'Help' without single quotes for a list of \
         commands.";
      cli builder

and help (builder : t) : State.t =
  print_endline
    ("List of commands (do not enter single quotes): \n"
   ^ "\t 'Help': Display a list of commands. \n"
   ^ "\t 'Add Body': Add a new celestial body to the scene. \n"
   ^ "\t 'Remove Body': Remove a body from the scene. \n"
   ^ "\t 'Set Position': Change the position of an existing body. \n"
   ^ "\t 'Set Color': Change the color of an existing body. \n"
   ^ "\t 'Set Velocity': Change the velocity of an existing body. \n"
   ^ "\t 'Set Radius': Change the radius of an existing body. \n"
   ^ "\t 'Set Mass': Change the mass of an existing body. \n"
   ^ "\t 'Save Scene': Save the scene to a file and play it. \n"
   ^ "\t 'Quit': Play the scene without saving.");
  cli builder

and add_body (builder : t) : State.t =
  let entered_name = string_prompt "Enter a (unique) name for the body:" in
  match Statebuilder.get_body entered_name builder with
  | None ->
      cli
        (Statebuilder.add_body
           (Body.make Constants.default_radius Constants.default_mass
              (Vector2.make Constants.default_pos_x Constants.default_pos_y)
              (Vector2.make Constants.default_vel_x Constants.default_vel_y)
              Constants.default_red Constants.default_green
              Constants.default_blue entered_name)
           builder)
  | Some _ ->
      print_endline ("The name '" ^ entered_name ^ "' is already in use.");
      add_body builder

and remove_body (builder : t) : State.t =
  let entered_name = string_prompt "Enter the name of the body to remove:" in
  match Statebuilder.get_body entered_name builder with
  | None ->
      print_endline
        ("There is no body named " ^ entered_name ^ ". No body was removed.");
      cli builder
  | Some b -> cli (Statebuilder.delete_body b builder)

and set_position (builder : t) : State.t =
  let entered_name = string_prompt "Enter the name of the body to move:" in
  match Statebuilder.get_body entered_name builder with
  | None ->
      print_endline
        ("There is no body named " ^ entered_name ^ ". No body will be moved.");
      cli builder
  | Some b ->
      let new_position = vec_prompt "Enter the new position:" in
      cli (Statebuilder.set_position b new_position builder)

and set_color (builder : t) : State.t =
  let entered_name =
    string_prompt "Enter the name of the body change colors:"
  in
  match Statebuilder.get_body entered_name builder with
  | None ->
      print_endline
        ("There is no body named " ^ entered_name
       ^ ". No body color will change.");
      cli builder
  | Some b ->
      let rd, gn, bl = color_prompt "Enter the new color:" in
      cli (Statebuilder.set_color b rd gn bl builder)

and set_velocity (builder : t) : State.t =
  let entered_name =
    string_prompt "Enter the name of the body to change velocity:"
  in
  match Statebuilder.get_body entered_name builder with
  | None ->
      print_endline
        ("There is no body named " ^ entered_name
       ^ ". No velocity will be changed.");
      cli builder
  | Some b ->
      let new_velocity = vec_prompt "Enter the new velocity:" in
      cli (Statebuilder.set_velocity b new_velocity builder)

and set_radius (builder : t) : State.t =
  let entered_name =
    string_prompt "Enter the name of the body to change radius:"
  in
  match Statebuilder.get_body entered_name builder with
  | None ->
      print_endline
        ("There is no body named " ^ entered_name
       ^ ". No radius will be changed.");
      cli builder
  | Some b ->
      let new_radius = float_prompt "Enter the new radius:" true in
      cli (Statebuilder.set_radius b new_radius builder)

and set_mass (builder : t) : State.t =
  let entered_name =
    string_prompt "Enter the name of the body to change mass:"
  in
  match Statebuilder.get_body entered_name builder with
  | None ->
      print_endline
        ("There is no body named " ^ entered_name ^ ". No mass will be changed.");
      cli builder
  | Some b ->
      let new_mass = float_prompt "Enter the new mass:" true in
      cli (Statebuilder.set_mass b new_mass builder)

and save_scene (builder : t) : State.t =
  let entered_name = string_prompt "Enter the file name to save the file to:" in
  let json = Statebuilder.to_json builder in
  Yojson.Basic.to_file (Constants.scene_path entered_name) json;
  Statebuilder.to_state builder

let session = Statebuilder.empty
let start_session s = cli s
