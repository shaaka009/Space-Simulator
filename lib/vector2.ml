type t = {
  x : float;
  y : float;
}

exception ZeroVector

let make (a : float) (b : float) : t = { x = a; y = b }
let right : t = make 1. 0.
let left : t = make ~-.1. 0.
let up : t = make 0. 1.
let down : t = make 0. ~-.1.
let zero : t = make 0. 0.
let square_magnitude (vec : t) = vec.x *. vec.x |> ( +. ) (vec.y *. vec.y)
let magnitude (vec : t) : float = square_magnitude vec |> sqrt

let compare_magnitude (vec1 : t) (vec2 : t) : int =
  Float.compare (square_magnitude vec1) (square_magnitude vec2)

let scale (c : float) (vec : t) : t = { x = c *. vec.x; y = c *. vec.y }
let ( *< ) = scale
let ( ~-< ) (vec1 : t) : t = ~-.1. *< vec1

let normalize (a : t) : t =
  match magnitude a with
  | m when m = 0. -> raise ZeroVector
  | m -> 1. /. m *< a

let add (vec1 : t) (vec2 : t) : t =
  { x = vec1.x +. vec2.x; y = vec1.y +. vec2.y }

let ( +< ) = add
let subtract (vec1 : t) (vec2 : t) : t = vec1 +< ~-<vec2
let ( -< ) = subtract
let distance (a : t) (b : t) = magnitude (b -< a)
let dot (vec1 : t) (vec2 : t) : float = (vec1.x *. vec2.x) +. (vec1.y *. vec2.y)
