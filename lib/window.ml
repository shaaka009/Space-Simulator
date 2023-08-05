open Graphics

type t = unit

(**Width of the window*)
let width = 1200

(**Height of the window*)
let height = 800

(**The x coordinate of the left wall of the legend box*)
let left_bound = 980

(**The y coordinate of the bottom wall of the legend box*)
let bottom_bound = if Constants.tilt_implement then 460 else 530

(**Center x coordinate*)
let center_x = width / 2

(**Center y coordinate*)
let center_y = height / 2

(**Vector pointing to the center of the display*)
let center_vector = Vector2.make (Int.to_float center_x) (Int.to_float center_y)

(**String that is inputed as argument for Graphics.open_graph *)
let make_string = " " ^ Int.to_string width ^ "x" ^ Int.to_string height

(**A reference to a list of star points, constant after creation*)
let stars_location : (int * int) list ref = ref []

(**Makes the list of random coordinates that are stars*)
let get_stars =
  for i = 0 to 600 do
    let _ = i in
    stars_location := (Random.int width, Random.int height) :: !stars_location
  done

let completed = ref true
let json_name = ref ""

let run_menu () : string =
  (*Drawing black screen*)
  let _ = Graphics.set_color (Graphics.rgb 255 255 255) in
  let _ = Graphics.(fill_rect 0 0 (size_x ()) (size_y ())) in
  (*Drawing Title*)
  let _ = Graphics.moveto 525 775 in
  let _ = Graphics.set_color (Graphics.rgb 0 0 0) in
  (*let _ = Graphics.set_font "Arial" in*)
  let _ = Graphics.draw_string "Welcome to Space Simulator" in
  let _ = Graphics.moveto 500 750 in
  let _ = Graphics.draw_string "Choose your scene, or make your own:" in
  (*Drawing scene list*)
  let scenes0 = Sys.readdir "scenes" in
  let scenes = Array.append scenes0 [| "Make your own" |] in
  let _ =
    for i = 0 to Array.length scenes - 1 do
      let _ = Graphics.moveto 500 (720 - (i * 20)) in
      let _ =
        Graphics.draw_string ("[" ^ string_of_int i ^ "] " ^ scenes.(i))
      in
      ()
    done
  in
  while !completed do
    let _ =
      if Graphics.button_down () then
        let x, y = Graphics.mouse_pos () in
        let index =
          36. -. (float_of_int y /. 20.) |> Float.round |> int_of_float
        in
        if (-1 < index && index < Array.length scenes) && 450 < x && x < 700
        then (
          json_name := scenes.(index);
          completed := false)
        else ()
      else ()
    in
    ()
  done;
  !json_name

(**)
let draw_state_simple (state : State.t) : t =
  let _ = set_color (rgb 0 0 0) in
  let _ = fill_rect 0 0 (size_x ()) (size_y ()) in
  let _ =
    List.iter
      (fun (x, y) ->
        let z = Random.int 100 + 50 in
        let _ = set_color (rgb z z z) in
        Graphics.plot x y)
      !stars_location
  in
  let _ = Graphics.(set_color (rgb 255 129 0)) in
  List.iter
    (fun z ->
      let _ =
        Graphics.set_color (rgb (Body.red z) (Body.green z) (Body.blue z))
      in
      let _ =
        Graphics.fill_circle
          (Float.to_int (Body.position z).x / 2)
          (Float.to_int (Body.position z).y / 2)
          (Float.to_int (Body.radius z) / 2)
      in
      let _ = Graphics.set_color (rgb 0 0 0) in
      let _ =
        Graphics.draw_circle
          (Float.to_int (Body.position z).x / 2)
          (Float.to_int (Body.position z).y / 2)
          (Float.to_int (Body.radius z) / 2)
      in
      if true then
        let _ = Graphics.set_color (rgb 255 255 255) in
        let posx, posy =
          ( Float.to_int ((Body.position z).x /. 2.)
            - (String.length (Body.name z) / 2 * 7),
            Float.to_int
              (((Body.position z).y /. 2.) +. (Body.radius z /. 2.) +. 11.) )
        in
        let _ = Graphics.moveto posx posy in
        Graphics.draw_string (Body.name z)
      else ())
    (State.bodies state)

(*Draws the current inputed state, as well as the legend and total time passed
  in simulation*)
let draw_state state (total_time : int) (tilt : float) (draw_names : bool) =
  let _ = state in
  let _ = Graphics.clear_graph in
  let _ = set_color (rgb 0 0 0) in
  let _ = fill_rect 0 0 (size_x ()) (size_y () - 30) in
  let _ =
    List.iter
      (fun (x, y) ->
        let z = Random.int 100 + 50 in
        let _ = set_color (rgb z z z) in
        Graphics.plot x y)
      !stars_location
  in
  let _ = Graphics.(set_color (rgb 255 129 0)) in
  let _ =
    List.iter
      (fun z ->
        let _ =
          Graphics.set_color (rgb (Body.red z) (Body.green z) (Body.blue z))
        in
        let _ =
          Graphics.fill_circle
            (Float.to_int (Body.position z).x)
            (Float.to_int (Body.position z).y)
            (Float.to_int (Body.radius z))
        in
        let _ = Graphics.set_color (rgb 0 0 0) in
        let _ =
          Graphics.draw_circle
            (Float.to_int (Body.position z).x)
            (Float.to_int (Body.position z).y)
            (Float.to_int (Body.radius z))
        in
        if draw_names then
          let _ = Graphics.set_color (rgb 255 255 255) in
          let posx, posy =
            ( Float.to_int (Body.position z).x
              - (String.length (Body.name z) / 2 * 7),
              Float.to_int ((Body.position z).y +. Body.radius z +. 11.) )
          in
          let _ = Graphics.moveto posx posy in
          Graphics.draw_string (Body.name z)
        else ())
      (State.bodies state)
  in
  let _ = Graphics.set_color (rgb 50 50 50) in
  let _ =
    Graphics.fill_rect left_bound bottom_bound
      (size_x () - left_bound)
      (size_y () - bottom_bound - 30)
  in
  let _ = Graphics.fill_rect 0 (size_y () - 30) (size_x ()) 30 in
  let _ = Graphics.set_color (rgb 255 255 255) in
  let _ = Graphics.moveto 10 778 in
  let _ =
    Graphics.draw_string
      ("Time Elapsed: " ^ Int.to_string total_time
     ^ " seconds                    Tilt: " ^ Float.to_string tilt ^ " radians"
      )
  in
  let _ = Graphics.moveto 985 750 in
  let _ = Graphics.draw_string "Input             Effect" in
  let _ = Graphics.moveto 995 710 in
  let _ = Graphics.draw_string "p             Pause/Play" in
  let _ = Graphics.moveto 995 680 in
  let _ = Graphics.draw_string "a              Speed up" in
  let _ = Graphics.moveto 995 650 in
  let _ = Graphics.draw_string "d              Slow down" in
  let _ = Graphics.moveto 995 620 in
  let _ = Graphics.draw_string "o      Return to original speed" in
  let _ = Graphics.moveto 995 590 in
  let _ = Graphics.draw_string "r               Restart" in
  let _ = Graphics.moveto 995 560 in
  let _ = Graphics.draw_string "n              Show Names" in
  if Constants.tilt_implement then
    let _ = Graphics.moveto 995 530 in
    let _ = Graphics.draw_string "b              tilt back" in
    let _ = Graphics.moveto 995 500 in
    let _ = Graphics.draw_string "f             tilt forward" in
    let _ = Graphics.moveto 995 470 in
    Graphics.draw_string "q                Quit"
  else
    let _ = Graphics.moveto 995 530 in
    Graphics.draw_string "q                Quit"

(*Start and end code*)
let make () =
  let _ = get_stars in
  Graphics.open_graph make_string

(**Closes the window*)
let close_window () = Graphics.close_graph ()
