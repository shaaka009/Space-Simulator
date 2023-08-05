type t = {
  position : Vector2.t;
  z_pos : float;
  radius : float;
  mass : float;
  velocity : Vector2.t;
  r : int;
  g : int;
  b : int;
  name : string;
}

exception ZeroMass

let gravitational_constant = Constants.gravitational_constant

let make rad m pos vel rd gn bl n =
  {
    position = pos;
    z_pos = 0.;
    radius = rad;
    mass = m;
    velocity = vel;
    r = rd;
    g = gn;
    b = bl;
    name = n;
  }

let radius a = a.radius
let mass a = a.mass
let position a = a.position
let z_pos a = a.z_pos
let velocity a = a.velocity
let name a = a.name
let red a = a.r
let green a = a.g
let blue a = a.b
let distance a b = Vector2.distance a.position b.position
let move_to (pos : Vector2.t) (a : t) : t = { a with position = pos }
let set_z (a : t) (z : float) : t = { a with z_pos = z }
let set_velocity (vel : Vector2.t) (a : t) : t = { a with velocity = vel }
let set_name (n : string) (a : t) : t = { a with name = n }

let move_for_time (time : float) (a : t) : t =
  move_to Vector2.(a.velocity |> scale time |> add a.position) a

let force_for_time (force : Vector2.t) (time : float) (a : t) : t =
  if a.mass = 0. then raise ZeroMass
  else
    set_velocity Vector2.(force |> scale (time /. a.mass) |> add a.velocity) a

let collides (a : t) (b : t) : bool =
  if distance a b = 0. then false else distance a b < a.radius +. b.radius

let colliders (bodies : t list) : (t * t) list =
  let repeated_tups =
    List.concat_map
      (fun b1 ->
        List.map (fun b2 -> if collides b1 b2 then [ (b1, b2) ] else []) bodies
        |> List.flatten)
      bodies
  in
  List.fold_right
    (fun (a, b) l ->
      if
        List.mem true
          (List.map (fun (c, d) -> a = d || c = b || a = c || b = d) l)
      then l
      else (a, b) :: l)
    (List.rev repeated_tups) []

let separate ((a, b) : t * t) : t * t =
  if a |> collides b |> not then (a, b)
  else
    match distance a b with
    | 0. -> (a, b)
    | dist ->
        let half_intersection = (a.radius +. b.radius -. dist) /. 2. in
        let b_to_a =
          Vector2.(
            b.position |> scale ~-.1. |> add a.position |> normalize
            |> scale half_intersection)
        in
        let c = move_to Vector2.(b_to_a |> add a.position) a in
        let d = move_to Vector2.(b_to_a |> scale ~-.1. |> add b.position) b in
        (c, d)

let elastic_separation ((a, b) : t * t) : t * t =
  if a |> collides b |> not then (a, b)
  else if a.mass +. b.mass = 0. then raise ZeroMass
  else
    let a, b = separate (a, b) in
    let v1f =
      Vector2.(
        ((a.mass -. b.mass) /. (a.mass +. b.mass) *< a.velocity)
        +< (2. *. b.mass /. (a.mass +. b.mass) *< b.velocity))
    in
    let v2f =
      Vector2.(
        (2. *. a.mass /. (a.mass +. b.mass) *< a.velocity)
        +< ((b.mass -. a.mass) /. (a.mass +. b.mass) *< b.velocity))
    in
    (set_velocity v1f a, set_velocity v2f b)

let inelastic_separation ((a, b) : t * t) : t * t =
  if a |> collides b |> not then (a, b)
  else if a.mass +. b.mass = 0. then raise ZeroMass
  else
    let a, b = separate (a, b) in
    let vf =
      Vector2.(
        1. /. (a.mass +. b.mass)
        *< ((a.mass *< a.velocity) +< (b.mass *< b.velocity)))
    in
    (set_velocity vf a, set_velocity vf b)

let gravity_for_time (bodies : t list) (body : t) (time : float) : t =
  let forces =
    bodies
    |> List.map (fun b ->
           if distance body b = 0. then Vector2.zero
           else
             Vector2.make
               (b.position.x -. body.position.x)
               (b.position.y -. body.position.y)
             |> Vector2.normalize
             |> Vector2.scale
                  (gravitational_constant
                  *. (body.mass *. b.mass
                     /. (distance body b *. distance body b))))
  in
  let sum_of_forces = forces |> List.fold_left Vector2.add Vector2.zero in
  let body_new_velocity =
    { body with velocity = (force_for_time sum_of_forces time body).velocity }
  in
  {
    body_new_velocity with
    position = (move_for_time time body_new_velocity).position;
  }
