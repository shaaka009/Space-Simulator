open SolarSystem

(**Contains the current state*)
let state = ref (State.init_state [])

(*Initial state*)
let initial = ref (State.init_state [])

(**the start tilt of the display, saved for correct reseting*)
let start_deg = (Float.pi /. 2.) +. (Float.pi /. 10.)

(**the current tilt of the display*)
let deg = ref start_deg

(**The starting display rate*)
let start_display_rate = 10

(**Amount of time in seconds that a discrete step is taken in calculations*)
let computation_rate = ref (1. /. 30.)

(**The time between the redrawing of frames*)
let framerate = 1. /. 144.

(**Number of computations before redrawing frame*)
let current_display_rate = ref 0

(**Helper reference variable to unpause at right speed (there might be a better
   way to do this)*)
let store_display_rate = ref 0

(**How much display rate changes by when user slows down or speeds up *)
let display_incr = start_display_rate / 10

(**The total amount of time that has passed in the simulation*)
let total_time = ref 0.

(** Lets the system know if a body is currently being displayed in the info box*)
let displaying_info = ref false

(** Keeps the system updated on what body is being displayed on the info card *)
let displayed_body = ref (Body.make 0. 0. Vector2.zero Vector2.zero 0 0 0 "")

(** Boolean that tells whether to print names on screen *)
let show_names = ref true

(** A map of points that designate the corners of a legend that indeicates the
    tilt angle *)

let map = ref [| (15, 745); (50, 780); (85, 745); (50, 710) |]

(**Takes in a user input character and makes appropriate adjustments to
   simulation*)
let check_stat inp =
  match inp with
  | 'p' ->
      if !current_display_rate = 0 then
        current_display_rate := !store_display_rate
      else
        let _ = store_display_rate := !current_display_rate in
        current_display_rate := 0
  | 'a' -> current_display_rate := !current_display_rate + display_incr
  | 'd' ->
      if !current_display_rate = 0 then ()
      else current_display_rate := !current_display_rate - display_incr
  | 'o' -> current_display_rate := start_display_rate
  | 'r' ->
      let _ = state := !initial in
      let _ = deg := start_deg in
      total_time := 0.
  | 'q' -> exit 0
  | 'b' -> deg := !deg +. (Float.pi /. 100.)
  | 'f' -> deg := !deg -. (Float.pi /. 100.)
  | 'n' -> show_names := not !show_names
  | _ -> ()

(** Draws the information card for the current planet *)
let draw_card (bd : Body.t) : unit =
  Graphics.set_color (Graphics.rgb 0 0 0);
  Graphics.fill_rect 0 60 180 290;
  Graphics.set_color (Graphics.rgb 120 120 120);
  Graphics.draw_rect 0 60 180 290;
  let _ =
    Graphics.set_color
      (Graphics.rgb (Body.red bd) (Body.green bd) (Body.blue bd))
  in
  let _ = Graphics.fill_circle 90 280 45 in
  let _ = Graphics.set_color (Graphics.rgb 0 0 0) in
  let _ = Graphics.draw_circle 90 280 45 in
  let _ = Graphics.set_color (Graphics.rgb 255 255 255) in
  let _ = Graphics.moveto 24 205 in
  let _ = Graphics.draw_string ("Name: " ^ Body.name bd) in
  let _ = Graphics.moveto 24 185 in
  let _ =
    Graphics.draw_string ("Mass: " ^ Printf.sprintf "%g" (Body.mass bd) ^ "kg")
  in
  let _ = Graphics.moveto 24 165 in
  let _ =
    Graphics.draw_string
      ("Radius: " ^ Printf.sprintf "%g" (Body.radius bd) ^ "m")
  in
  let _ = Graphics.moveto 24 145 in
  let _ =
    Graphics.draw_string
      ("Velocity: "
      ^ Printf.sprintf "%g" (Vector2.magnitude (Body.velocity bd))
      ^ "m/s")
  in
  let _ = Graphics.moveto 24 125 in
  let _ =
    Graphics.draw_string
      ("X Position: " ^ Printf.sprintf "%g" (Body.position bd).x)
  in
  let _ = Graphics.moveto 24 105 in
  let _ =
    Graphics.draw_string
      ("Y Position: " ^ Printf.sprintf "%g" (Body.position bd).y)
  in
  ()

(** Gets the right planet for the info card and makes sure all info regarding
    drawing the card is correct*)
let draw_info ((x, y) : int * int) (state : State.t) : unit =
  let bodies = State.bodies state in
  let clicked =
    List.find_opt
      (fun a ->
        if
          Vector2.distance (Body.position a)
            (Vector2.make (float_of_int x) (float_of_int y))
          < Body.radius a +. 8.
        then true
        else false)
      bodies
  in
  match clicked with
  | None -> displaying_info := false
  | Some a ->
      displaying_info := true;
      displayed_body := a;
      draw_card !displayed_body

(**Runs the main simulation*)
let run z : unit =
  let _ = Graphics.auto_synchronize false in

  (* Prints the inital state; this waits for first input *)
  let _ =
    Window.draw_state z 0
      (Int.to_float (Float.to_int (!deg *. 1000.)) /. 1000.)
      !show_names
  in
  Graphics.synchronize ();

  (* Waits for first user input, either play or quit *)
  let stat = ref (Graphics.wait_next_event [ Key_pressed ]) in

  while !stat.key != 'p' && !stat.key != 'q' do
    stat := Graphics.wait_next_event [ Key_pressed ]
  done;

  if !stat.key = 'q' then exit 0
  else if !stat.key = 'p' then current_display_rate := start_display_rate;

  (* Main loop. Should only end with user input of 'q' *)
  let _ =
    while 1 < 2 do
      Graphics.clear_graph ();
      let t = Sys.time () in

      (* If tilt implementation is active, get tilt state, otherwise get regular
         state *)
      let tilted =
        if Constants.tilt_implement then
          State.tilt !state
            (Int.to_float (Float.to_int (!deg *. 1000.)) /. 1000.)
        else !state
      in

      (* Draw the current state with correct information *)
      let _ =
        Window.draw_state tilted (Float.to_int !total_time)
          (if Constants.tilt_implement then
           Int.to_float (Float.to_int (!deg *. 1000.)) /. 1000.
          else 0.)
          !show_names
      in

      map :=
        [|
          (15, 725);
          (50, 725 + int_of_float (Stdlib.cos !deg *. 35.));
          (85, 725);
          (50, 725 - int_of_float (Stdlib.cos !deg *. 35.));
        |];
      Graphics.set_color (Graphics.rgb 255 0 0);
      Graphics.draw_poly !map;
      for i = 0 to Array.length !map - 1 do
        let x, y = !map.(i) in
        Graphics.fill_circle x y 4
      done;

      (* Check for user inputs *)
      let stat = Graphics.key_pressed () in
      if stat then check_stat (Graphics.read_key ());

      if Graphics.button_down () then
        let mouse_pos = Graphics.mouse_pos () in
        draw_info mouse_pos tilted
      else ();
      if !displaying_info then
        let _ =
          displayed_body :=
            List.find
              (fun a ->
                if Body.name a = Body.name !displayed_body then true else false)
              (State.bodies !state)
        in
        draw_card !displayed_body
      else ();
      (* Update state information as well as state itself *)
      for i = 1 to !current_display_rate do
        let _ = i in
        state := State.run !computation_rate !state;
        total_time := !total_time +. !computation_rate
      done;
      Graphics.synchronize ();
      while Sys.time () -. t < framerate do
        ()
      done
    done
  in
  ()

(**Runs this file*)
let _ =
  try
    let w = Window.make () in
    let _ = Graphics.set_window_title "Space Physics Simulator" in
    let _ = Graphics.remember_mode false in
    let s = Window.run_menu () in
    let _ = Window.close_window w in
    let _ = Graphics.open_graph " 600x400" in
    let j =
      if s = "Make your own" then Clibuilder.(start_session session)
      else
        Statebuilder.from_json
          (Yojson.Basic.from_file (Constants.scene_path_2 s))
        |> Statebuilder.to_state
    in
    let _ = state := j in
    let _ = initial := j in
    let _ = Graphics.close_graph () in
    let w = Window.make () in
    let _ = Graphics.set_window_title "Space Physics Simulator" in
    let _ = Graphics.remember_mode false in
    let _ = run j in
    let _ = Window.close_window w in
    ()
  with Graphics.Graphic_failure "fatal I/O error" -> ()
