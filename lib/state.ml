type t = { bodies : Body.t list }

let init_state (lst : Body.t list) : t = { bodies = lst }

let body_to_json (body : Body.t) : Yojson.Basic.t =
  let r = ("radius", `Float (Body.radius body)) in
  let m = ("mass", `Float (Body.mass body)) in
  let px = ("position_x", `Float (Body.position body).x) in
  let py = ("position_y", `Float (Body.position body).y) in
  let vx = ("velocity_x", `Float (Body.velocity body).x) in
  let vy = ("velocity_y", `Float (Body.velocity body).y) in
  let rd = ("red", `Int (Body.red body)) in
  let gn = ("green", `Int (Body.green body)) in
  let bl = ("blue", `Int (Body.blue body)) in
  let n = ("name", `String (Body.name body)) in
  `Assoc [ r; m; px; py; vx; vy; rd; gn; bl; n ]

let to_json (state : t) : Yojson.Basic.t =
  `Assoc [ ("bodies", `List (List.map body_to_json state.bodies)) ]

let run (time : float) (state : t) : t =
  let collision_bodies =
    let a, b = state.bodies |> Body.colliders |> List.split in
    a @ b
  in
  let non_colliders =
    List.filter (fun a -> not (List.mem a collision_bodies)) state.bodies
  in
  let separated_colliders_list =
    if Constants.elastic_collisions then
      match
        Body.colliders state.bodies
        |> List.map Body.elastic_separation
        |> List.split
      with
      | a, b -> a @ b
    else
      match
        Body.colliders state.bodies
        |> List.map Body.inelastic_separation
        |> List.split
      with
      | a, b -> a @ b
  in
  let new_b = separated_colliders_list @ non_colliders in
  let new_t =
    List.map (fun body -> Body.gravity_for_time new_b body time) new_b
  in
  { bodies = new_t }

let order (state : t) : t =
  {
    bodies =
      List.sort
        (fun a b ->
          if Body.z_pos a = Body.z_pos b then 0
          else
            match Body.z_pos a > Body.z_pos b with
            | true -> 1
            | false -> -1)
        state.bodies;
  }

let dist_from_camera (v : Vector2.t) z_pos =
  Stdlib.sqrt
    (((v.x -. 600.) *. (v.x -. 600.))
    +. ((v.y -. 400.) *. (v.y -. 400.))
    +. ((z_pos -. 1200.) *. (z_pos -. 1200.)))

let tilt (state : t) (rad : float) : t =
  order
    {
      bodies =
        List.map
          (fun z ->
            let mass = Body.mass z in
            let position =
              Vector2.( +< )
                (Vector2.make
                   (Vector2.( -< ) (Body.position z) (Vector2.make 600. 400.)).x
                   ((Vector2.( -< ) (Body.position z) (Vector2.make 600. 400.))
                      .y *. Stdlib.cos (-.rad)
                   +. (Body.z_pos z *. ~-.(Stdlib.sin (-.rad)))))
                (Vector2.make 600. 400.)
            in
            let vel = Body.velocity z in
            let red = Body.red z in
            let green = Body.green z in
            let blue = Body.blue z in
            let name = Body.name z in
            let z_pos =
              (Vector2.( -< ) (Body.position z) (Vector2.make 600. 400.)).y
              *. ~-.(Stdlib.sin (-.rad))
              +. (Body.z_pos z *. Stdlib.cos (-.rad))
            in
            let rad =
              if
                Body.radius z *. (1200. /. dist_from_camera position z_pos) < 0.
              then 0.
              else Body.radius z *. (1200. /. dist_from_camera position z_pos)
            in
            Body.set_z
              (Body.make rad mass position vel red green blue name)
              z_pos)
          state.bodies;
    }

let bodies (state : t) : Body.t list = state.bodies
