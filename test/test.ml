(* Test Plan: vector2.ml was tested automatically using OUnit. We used a
   combination of black box and glass box testing for this module. We first
   created tests solely based on the vector2.mli specification. Then, we used
   Bisect to analyze the quality of code coverage and wrote glass box tests to
   improve coverage.

   body.ml was also tested automatically using OUnit. We again used a
   combination of black box testing and glass box testing. We wrote black-box
   tests for the module using the specification provided in body.mli and then
   used Bisect to determine where more testing was needed.

   state.ml was also tested automatically using OUnit. This module was primarily
   testing using black box testing. We used the specification provided in
   state.mli and did physics calculations by hand to determine the expected
   values.

   statebuilder.ml was tested automatically using OUnit. We used a combination
   of black box and glass box testing for this module. We created tests using
   the specifications found in the statebuilder.mli file and then used Bisect to
   create glass box tests for improved coverage.

   clibuilder.ml was tested manually. This module creates a command line
   interface that requires user interaction, so we tested this module by
   playtesting the program. We would play the program and then fix unexpected
   behavior.

   window.ml was also tested manually. This module creates a graphical window
   that also requires user interaction, so we tested the program by playtesting.
   We ran the program to test if the program reacted as we expected, and fixed
   unexpected bugs.

   This testing approach demonstrates the correctness of our system because most
   of our modules were tested automatically. We used black-box testing to test
   our program without the bias of knowing how the code would processes the
   information behind the scenes. We also used glass-box testing and Bisect to
   ensure that a relatively large proportion of our methods were tested. By
   ensuring that each part of our program is tested on its own, we have improved
   the chances that the system will work as a whole. We also playtested modules
   that could not be automatically tested. This meant that we spent time
   searching for unexpected behavior and fixing bugs. By taking the client's
   role in playtesting, we can catch bugs before the client can run into
   them. *)

open OUnit2
open SolarSystem

(* [floats_equal a b] is the boolean representing if [a] is approximately [b] *)
let floats_equal a b = abs_float (a -. b) < 0.0001

(* [vecs_equal v w] is the boolean representing if [v] is approximately [w] *)
let vecs_equal (a : Vector2.t) (b : Vector2.t) : bool =
  floats_equal a.x b.x && floats_equal a.y b.y

(* [string_of_vec v] is the string representation of vector [v] *)
let string_of_vec (vec : Vector2.t) : string =
  "(" ^ string_of_float vec.x ^ ", " ^ string_of_float vec.y ^ ")"

let vector_test name vec expected : test =
  name >:: fun _ ->
  assert_equal expected vec ~cmp:vecs_equal ~printer:string_of_vec

(* [magnitude_test n v e] constructs a unit test named [n] that tests if
   [Vector2.magnitude v] is [e] *)
let magnitude_test name vec expected : test =
  name >:: fun _ ->
  assert_equal expected
    Vector2.(magnitude vec)
    ~cmp:floats_equal ~printer:string_of_float

(* [square_magnitude_test n v e] constructs a unit test named [n] that tests if
   [Vector2.square_magnitude v] is [e] *)
let square_magnitude_test name vec expected : test =
  name >:: fun _ ->
  assert_equal expected
    Vector2.(square_magnitude vec)
    ~cmp:floats_equal ~printer:string_of_float

(* [sign n] is [-1] if [n < 0], [1] if [n > 0], and [0] otherwise*)
let sign n =
  match n with
  | _ when n < 0 -> -1
  | _ when n > 0 -> 1
  | _ -> 0

(* [compare_magnitude_test n v w e] constructs a unit test named [n] that tests
   if [Vector2.compare_magnitude v w] has the same sign as [e] *)
let compare_magnitude_test name vec1 vec2 expected : test =
  name >:: fun _ ->
  assert_equal (sign expected) Vector2.(compare_magnitude vec1 vec2 |> sign)

(* [dot_test n v w e] constructes a unit test named [n] that tests if
   [Vector2.dot v w] is [e] *)
let dot_test name vec1 vec2 expected : test =
  name >:: fun _ ->
  assert_equal expected
    Vector2.(dot vec1 vec2)
    ~cmp:floats_equal ~printer:string_of_float

(* [distance_test n v w e] constructs a unit test named [n] that tests if
   [Vector2.distance v w] is [e] *)
let distance_test name vec1 vec2 expected : test =
  name >:: fun _ ->
  assert_equal expected
    (Vector2.distance vec1 vec2)
    ~cmp:floats_equal ~printer:string_of_float

(* [normalize_test n v e r] constructs a unit test named [n] that tests if
   [Vector2.normalize v] [e]. If [r], then the test checks that
   [Vector2.normalize v] raises [Vector2.ZeroVector] *)
let normalize_test name vec expected raises_exception : test =
  if raises_exception then
    name >:: fun _ ->
    assert_raises Vector2.ZeroVector (fun _ -> Vector2.normalize vec)
  else vector_test name (Vector2.normalize vec) expected

(* [add_test n v w e] constructs a unit test named [n] that tests if
   [Vector2.add v w] is [e] *)
let add_test name vec1 vec2 expected : test =
  vector_test name Vector2.(add vec1 vec2) expected

(* [scale_test n v c e] constructs a unit test named [n] that tests if
   [Vector2.scale c v] is [e] *)
let scale_test name vec scalar expected : test =
  vector_test name (Vector2.scale scalar vec) expected

let vector2_tests =
  [
    magnitude_test "<-1, 0> has magnitude 1" Vector2.left 1.;
    magnitude_test "<0, 1> has magnitude 1" Vector2.up 1.;
    magnitude_test "<1, 0> has magnitude 1" Vector2.right 1.;
    magnitude_test "<0, -1> has magnitude 1" Vector2.down 1.;
    magnitude_test "A zero vector has magnitude 0" Vector2.zero 0.;
    magnitude_test "The vector <1,1> has magnitude sqrt(2)" (Vector2.make 1. 1.)
      (sqrt 2.);
    distance_test "The distance from the origin to <1,0> is 1" Vector2.zero
      Vector2.right 1.;
    distance_test "The distance from <0,1> to <0,-1> is 2" Vector2.up
      Vector2.down 2.;
    distance_test "The distance from <-1,-1> to <1,1> is 2*sqrt(2)"
      (Vector2.make ~-.1. ~-.1.) (Vector2.make 1. 1.)
      (2. *. sqrt 2.);
    normalize_test "The normalized version of <2,0> is <1,0>"
      (Vector2.make 2. 0.) Vector2.right false;
    normalize_test "The normalized version of <0,0.0001> is <0,1>"
      (Vector2.make 0. 0.0001) Vector2.up false;
    normalize_test "We cannot normalize the zero vector" Vector2.zero
      Vector2.down true;
    add_test "add <1,0> <-1,0> = <0,0>" Vector2.right Vector2.left Vector2.zero;
    add_test "add <1,0> <0,1> = <1,1>" Vector2.right Vector2.up
      (Vector2.make 1. 1.);
    add_test "add <-1, 0> <-1, 0> = <-2, 0>" Vector2.left Vector2.left
      Vector2.(make ~-.2. 0.);
    add_test "add <158, 3245> <-2345, 123> = <-2187, 3368>"
      Vector2.(make 158. 3245.)
      Vector2.(make ~-.2345. 123.)
      Vector2.(make ~-.2187. 3368.);
    add_test "add <25, 134> <25, 134> = scale 2. <25 134>"
      Vector2.(make 25. 134.)
      Vector2.(make 25. 134.)
      Vector2.(make 25. 134. |> scale 2.);
    add_test "add <-2897, 28345> <-2897, 28345> = scale 2. <-2897, 28345>"
      Vector2.(make ~-.2897. 28345.)
      Vector2.(make ~-.2897. 28345.)
      Vector2.(make ~-.2897. 28345. |> scale 2.);
    vector_test "<3, 7> +< <7, 3> = <10, 10>"
      Vector2.(make 3. 7. +< make 7. 3.)
      Vector2.(make 10. 10.);
    vector_test "<-5, 0> +< <5, 0> = <0, 0>"
      Vector2.(make ~-.5. 0. +< make 5. 0.)
      Vector2.zero;
    vector_test "<0, 0> +< <3, 0> = <3, 0>"
      Vector2.(zero +< make 3. 0.)
      Vector2.(make 3. 0.);
    vector_test "<5, 0> +< <0, 0> = <5, 0>"
      Vector2.(make 5. 0. +< zero)
      Vector2.(make 5. 0.);
    scale_test "8 * <-1,0> = <-8,0>" Vector2.left 8. (Vector2.make ~-.8. 0.);
    scale_test "0.2 * <5,5> = <1,1>" (Vector2.make 5. 5.) 0.2
      (Vector2.make 1. 1.);
    vector_test "-4 *< <-1, 0> = <4, 0>"
      Vector2.(-4. *< left)
      Vector2.(make 4. 0.);
    vector_test "0.1 *< <10, 10> = <1,1>"
      Vector2.(0.1 *< make 10. 10.)
      Vector2.(make 1. 1.);
    vector_test "subtract <0, 0> <1, 0> = <-1, 0>"
      Vector2.(subtract (make 0. 0.) right)
      Vector2.left;
    vector_test "subtract <1, 0> <0, 0> = <1, 0>"
      Vector2.(subtract right zero)
      Vector2.right;
    vector_test "subtract <5, 2> <6, 1> = <-1, 1>"
      Vector2.(subtract (make 5. 2.) (make 6. 1.))
      Vector2.(make ~-.1. 1.);
    vector_test "<2, 0> -< <0, 0> = <2, 0> = <2, 0>"
      Vector2.(make 2. 0. -< zero)
      Vector2.(make 2. 0.);
    vector_test "<0, 0> -< <1, 0> = <-1, 0>"
      Vector2.(zero -< right)
      Vector2.left;
    vector_test "<10, 2> -< <8, 5> = <2, -3>"
      Vector2.(make 10. 2. -< make 8. 5.)
      Vector2.(make 2. ~-.3.);
    vector_test "~-< <0, 0> = <0, 0>" Vector2.(~-<zero) Vector2.zero;
    vector_test "~-< <1, 0> = <-1, 0>" Vector2.(~-<right) Vector2.left;
    vector_test "~-< <2, 3> = <-2, -3>"
      Vector2.(~-<(make 2. 3.))
      Vector2.(make ~-.2. ~-.3.);
    dot_test "dot <0, 0> <15, 20> = 0" Vector2.zero (Vector2.make 15. 20.) 0.;
    dot_test "dot <-12, 20> <0, 0> = 0" (Vector2.make ~-.12. 20.) Vector2.zero
      0.;
    dot_test "dot <2, 0> <5, 1000> = 10"
      Vector2.(2. *< right)
      Vector2.(make 5. 1000.)
      10.;
    dot_test "dot <0, 3> <-500, 1> = 3"
      Vector2.(3. *< up)
      Vector2.(make ~-.500. 1.)
      3.;
    dot_test "dot <5, 2> <2, 1> = 12"
      Vector2.(make 5. 2.)
      Vector2.(make 2. 1.)
      12.;
    dot_test "dot <3, 2> <-1, 2> = 1"
      Vector2.(make 3. 2.)
      Vector2.(make ~-.1. 2.)
      1.;
    square_magnitude_test "square_magnitude <0, 0> = 0" Vector2.zero 0.;
    square_magnitude_test "square_magnitude <1, 0> = 1" Vector2.right 1.;
    square_magnitude_test "square_magnitude <-1, 0> = 1" Vector2.left 1.;
    square_magnitude_test "square_magnitude <0, 1> = 1" Vector2.up 1.;
    square_magnitude_test "square_magnitude <0, -1> = 1" Vector2.down 1.;
    square_magnitude_test "square_magnitude <1, 1> = 2" Vector2.(make 1. 1.) 2.;
    square_magnitude_test "square_magnitude of <15, 243> is magnitude squared"
      Vector2.(make 15. 243.)
      Vector2.(make 15. 243. |> magnitude |> fun x -> x *. x);
    square_magnitude_test
      "square_magnitude of <-2341, 5364> is magnitude squared"
      Vector2.(make ~-.2341. 5364.)
      Vector2.(make ~-.2341. 5364. |> magnitude |> fun x -> x *. x);
    square_magnitude_test
      "square_magnitude of <1234, -5768> is magnitude squared"
      Vector2.(make 1234. ~-.5768.)
      Vector2.(make 1234. ~-.5768. |> magnitude |> fun x -> x *. x);
    compare_magnitude_test "compare_magnitude <0, 0> <0, 0> is zero"
      Vector2.zero Vector2.zero 0;
    compare_magnitude_test "compare_magnitude <1, 0> <0, 1> is zero"
      Vector2.right Vector2.up 0;
    compare_magnitude_test "compare_magnitude <-1, 0> <0, -1> is zero"
      Vector2.left Vector2.down 0;
    compare_magnitude_test "compare_magnitdue <1, 0> <0, -1> is zero"
      Vector2.right Vector2.down 0;
    compare_magnitude_test "compare_magnitude <1, 0> <0, 0> is positive"
      Vector2.right Vector2.zero 1;
    compare_magnitude_test "compare_magnitude <0, 0> <1, 0> is negative"
      Vector2.zero Vector2.right ~-1;
    compare_magnitude_test "compare_magnitude <-1, 0> <0, 0> is positive"
      Vector2.left Vector2.zero 1;
    compare_magnitude_test "compare_magnitude <0, 0> <-1, 0> is negative"
      Vector2.zero Vector2.left ~-1;
    compare_magnitude_test "compare_magnitude <1.1, 0> <1, 0> is positive"
      Vector2.(make 1.1 0.)
      Vector2.right 1;
    compare_magnitude_test "compare_magnitude <0.9, 0> <1, 0> is negative"
      Vector2.(make 0.9 0.)
      Vector2.right ~-1;
    compare_magnitude_test "compare_magnitude <1, -1> <-1, 1> is zero"
      Vector2.(make 1. ~-.1.)
      Vector2.(make ~-.1. 1.)
      0;
    compare_magnitude_test "compare_magnitude <100, 99> <-99, -99> is positive"
      Vector2.(make 100. 99.)
      Vector2.(make ~-.99. ~-.99.)
      1;
    compare_magnitude_test "compare_magnitude <99, 99> <99, -100> is negative"
      Vector2.(make 99. 99.)
      Vector2.(make 99. ~-.100.)
      ~-1;
  ]

(* [body_equal b d] is the boolean representing if [b=d] *)
let body_equal body1 body2 : bool =
  if Body.(radius body1 |> floats_equal (radius body2) |> not) then false
  else if Body.(mass body1 |> floats_equal (mass body2) |> not) then false
  else if Body.(position body1 |> vecs_equal (position body2) |> not) then false
  else if Body.(velocity body1 |> vecs_equal (velocity body2) |> not) then false
  else if Body.(z_pos body1 |> floats_equal (z_pos body2) |> not) then false
  else if Body.(name body1 = name body2 |> not) then false
  else if Body.(red body1 = red body2 |> not) then false
  else if Body.(green body1 = green body2 |> not) then false
  else if Body.(blue body1 = blue body2 |> not) then false
  else true

(* [body_tuple_equal (b*d) (f*g)] is the boolean representing if [(b*d)=
   (f*g)] *)
let body_tuple_equal b1 b2 : bool =
  match b1 with
  | x, y -> (
      match b2 with
      | w, z -> body_equal x w && body_equal y z)

(* [body_tuple_list_equal l1 l2] is the boolean representing if [l1 = l2] *)

let body_tuple_list_equal lst1 lst2 : bool =
  try
    if List.mem false (List.map2 body_tuple_equal lst1 lst2) then false
    else true
  with Invalid_argument s -> if s = "yo" then true else false

(* [string_of_body b] is the string representing [body] *)
let string_of_body body : string =
  let m = "Mass: " ^ string_of_float (Body.mass body) ^ "." in
  let r = " Radius: " ^ string_of_float (Body.radius body) ^ "." in
  let p = " Position: " ^ string_of_vec (Body.position body) ^ "." in
  let v = " Velocity: " ^ string_of_vec (Body.velocity body) ^ "." in
  let z = " Z-Pos: " ^ string_of_float (Body.z_pos body) ^ "." in
  let rd = " Red: " ^ string_of_int (Body.red body) ^ "." in
  let gn = " Green: " ^ string_of_int (Body.green body) ^ "." in
  let bl = " Blue: " ^ string_of_int (Body.blue body) ^ "." in
  let n = " Name: " ^ Body.name body ^ "." in
  m ^ r ^ p ^ v ^ z ^ rd ^ gn ^ bl ^ n

(* [string_of_tuple_body_list l] is the string representing the list of bodies
   containted in [l], where [l] is a list of pairs of bodies *)

let string_of_tuple_body_list tuple_body_list : string =
  let tuplelist_to_bodylist =
    match tuple_body_list |> List.split with
    | a, b -> a @ b
  in
  let string_list = List.map string_of_body tuplelist_to_bodylist in
  "[" ^ List.fold_left (fun a b -> a ^ ";\n" ^ b) "" string_list ^ "]"

(* [body_test n b e] constructs a unit test named [n] that tests if [b] is
   [e] *)
let body_test name body expected : test =
  name >:: fun _ ->
  assert_equal expected body ~cmp:body_equal ~printer:string_of_body

(* [move_for_time_test n b t e] constructs a unit test named [n] that tests if
   [move_for_time t b] is e *)
let move_for_time_test name body time expected : test =
  body_test name Body.(move_for_time time body) expected

(* [force_for_time_test n b f t e r] constructs a unit test named [n] that tests
   if [force_for_time f t b] is [e]. If [r] then tests that the function raises
   ZeroMass *)
let force_for_time_test name body force time expected raises : test =
  if raises then
    name >:: fun _ ->
    assert_raises Body.ZeroMass (fun () -> Body.force_for_time force time body)
  else body_test name Body.(force_for_time force time body) expected

(* [body_distance_test n b1 b2 e] constructs a unit test named [n] that tests if
   [Body.distance b1 b2] is [e] *)
let body_distance_test name body1 body2 expected : test =
  distance_test name Body.(position body1) Body.(position body2) expected

(* [body_name_test n b e] constructs a unit test named [n] that tests that [b]
   has name [e] *)
let body_name_test name body expected : test =
  name >:: fun _ -> assert_equal expected Body.(name body)

(* [body_position_test n b e] constructs a unit test named [n] that tests if [b]
   is at position [e]*)
let body_position_test name body expected : test =
  vector_test name Body.(position body) expected

(* [body_z_pos_test n b e] constructs a unit test named [n] that tests if [b]
   has z position [e]*)
let body_z_pos_test name body expected : test =
  name >:: fun _ ->
  assert_equal expected
    Body.(z_pos body)
    ~cmp:floats_equal ~printer:string_of_float

(* [body_velocity_test n b e] constructs a unit test named [n] that tests if [b]
   has velocity [e] *)
let body_velocity_test name body expected : test =
  vector_test name Body.(velocity body) expected

(* [body_collides_test n b1 b2 e] constructs a unit test named [n] that tests if
   [Body.collides b1 b2] is [e]*)
let body_collides_test name body1 body2 expected : test =
  name >:: fun _ ->
  assert_equal expected Body.(collides body1 body2) ~printer:string_of_bool

(* [separate_test n b (e1, e2)] constructs a unit test name [n] that tests if
   [separate b] is (e1, e2) *)
let separate_test name bodies (e1, e2) : test =
  name >:: fun _ ->
  let r1, r2 = Body.separate bodies in
  let _ = assert_equal e1 r1 ~cmp:body_equal ~printer:string_of_body in
  assert_equal e2 r2 ~cmp:body_equal ~printer:string_of_body

(* [elastic_separate_test n b (e1, e2) r] constructs a unit test name [n] that
   tests if [elastic_separation b] is (e1, e2). If [r] is true then tests that
   the function raises ZeroMass *)
let elastic_separate_test name bodies (e1, e2) raises : test =
  if raises then
    name >:: fun _ ->
    assert_raises Body.ZeroMass (fun () -> Body.elastic_separation bodies)
  else
    name >:: fun _ ->
    let r1, r2 = Body.elastic_separation bodies in
    let _ = assert_equal e1 r1 ~cmp:body_equal ~printer:string_of_body in
    assert_equal e2 r2 ~cmp:body_equal ~printer:string_of_body

(* [inelastic_separate_test n b (e1, e2) r] constructs a unit test name [n] that
   tests if [inelastic_separation b] is (e1, e2). If [r] is true then tests that
   the function raises ZeroMass *)
let inelastic_separate_test name bodies (e1, e2) raises : test =
  if raises then
    name >:: fun _ ->
    assert_raises Body.ZeroMass (fun () -> Body.inelastic_separation bodies)
  else
    name >:: fun _ ->
    let r1, r2 = Body.inelastic_separation bodies in
    let _ = assert_equal e1 r1 ~cmp:body_equal ~printer:string_of_body in
    assert_equal e2 r2 ~cmp:body_equal ~printer:string_of_body

let unit_body = Body.make 1. 1. Vector2.zero Vector2.zero 0 0 0 "u"

let unit_body_at_center =
  Body.make 1. 1. Window.center_vector Vector2.zero 0 0 0 "u"

let motion_body = Body.make 1. 1. Vector2.zero Vector2.up 0 0 0 "m"
let massless_body = Body.make 5. 0. Vector2.zero Vector2.zero 0 0 0 ""

let body_tests =
  [
    move_for_time_test "Motion shouldn't move in 0.s" motion_body 0. motion_body;
    move_for_time_test "Motion should move 1. up in 1.s" motion_body 1.
      (Body.move_to Vector2.up motion_body);
    move_for_time_test "Motion should move 0.5 up in 0.5s" motion_body 0.5
      (Body.move_to Vector2.(up |> scale 0.5) motion_body);
    move_for_time_test "Motion should move 5 up in 5s" motion_body 5.
      (Body.move_to Vector2.(up |> scale 5.) motion_body);
    force_for_time_test "Unit should have velocity 1. after 1.s of 1 unit force"
      unit_body Vector2.up 1.
      Body.(motion_body |> set_name (name unit_body))
      false;
    force_for_time_test "Force can slow an object" motion_body Vector2.down 1.
      Body.(unit_body |> set_name (name motion_body))
      false;
    force_for_time_test "Heavier objects accelerate half as fast"
      (Body.make 2. 2. Vector2.zero Vector2.zero 0 0 0 ".")
      Vector2.left 1.
      (Body.make 2. 2. Vector2.zero Vector2.(left |> scale 0.5) 0 0 0 ".")
      false;
    force_for_time_test "Zero force doesn't accelerate" unit_body Vector2.zero
      10000. unit_body false;
    force_for_time_test "Force for 0.s doesn't accelerate" unit_body
      Vector2.right 0. unit_body false;
    force_for_time_test "If the body has no mass it should raise an exception"
      massless_body Vector2.zero 0. unit_body true;
    separate_test "Separating bodies on x-axis"
      ( Body.make 2. 2. Vector2.right Vector2.zero 0 0 0 "",
        Body.make 2. 2. Vector2.left Vector2.zero 0 0 0 "" )
      ( Body.make 2. 2. Vector2.(right |> scale 2.) Vector2.zero 0 0 0 "",
        Body.make 2. 2. Vector2.(left |> scale 2.) Vector2.zero 0 0 0 "" );
    separate_test "Separating bodies on y-axis"
      ( Body.make 2. 2. Vector2.up Vector2.zero 0 0 0 "",
        Body.make 2. 2. Vector2.down Vector2.zero 0 0 0 "" )
      ( Body.make 2. 2. Vector2.(up |> scale 2.) Vector2.zero 0 0 0 "",
        Body.make 2. 2. Vector2.(down |> scale 2.) Vector2.zero 0 0 0 "" );
    elastic_separate_test "Elastically Separating bodies on x-axis"
      ( Body.make 2. 2. Vector2.right Vector2.zero 0 0 0 "",
        Body.make 2. 2. Vector2.left Vector2.zero 0 0 0 "" )
      ( Body.make 2. 2. Vector2.(right |> scale 2.) Vector2.zero 0 0 0 "",
        Body.make 2. 2. Vector2.(left |> scale 2.) Vector2.zero 0 0 0 "" )
      false;
    elastic_separate_test "Elastically Separating bodies on y-axis"
      ( Body.make 2. 2. Vector2.up Vector2.zero 0 0 0 "",
        Body.make 2. 2. Vector2.down Vector2.zero 0 0 0 "" )
      ( Body.make 2. 2. Vector2.(up |> scale 2.) Vector2.zero 0 0 0 "",
        Body.make 2. 2. Vector2.(down |> scale 2.) Vector2.zero 0 0 0 "" )
      false;
    elastic_separate_test "Elastically Separating moving bodies on x-axis"
      ( Body.make 2. 2. Vector2.right Vector2.left 0 0 0 "",
        Body.make 2. 2. Vector2.left Vector2.right 0 0 0 "" )
      ( Body.make 2. 2. Vector2.(right |> scale 2.) Vector2.right 0 0 0 "",
        Body.make 2. 2. Vector2.(left |> scale 2.) Vector2.left 0 0 0 "" )
      false;
    elastic_separate_test "Elastically Separating moving bodies on y-axis"
      ( Body.make 2. 2. Vector2.up Vector2.down 0 0 0 "",
        Body.make 2. 2. Vector2.down Vector2.up 0 0 0 "" )
      ( Body.make 2. 2. Vector2.(up |> scale 2.) Vector2.up 0 0 0 "",
        Body.make 2. 2. Vector2.(down |> scale 2.) Vector2.down 0 0 0 "" )
      false;
    elastic_separate_test "Test that elastic separation raises ZeroMass"
      (massless_body, Body.move_to Vector2.right massless_body)
      (massless_body, massless_body)
      true;
    elastic_separate_test
      "Elastic separation should do nothing if there is no collision"
      (unit_body, Body.move_to Vector2.(30. *< right) motion_body)
      (unit_body, Body.move_to Vector2.(30. *< right) motion_body)
      false;
    elastic_separate_test
      "Elastic separation should do nothing on non colliding massless bodies"
      (massless_body, Body.move_to Vector2.(50. *< right) massless_body)
      (massless_body, Body.move_to Vector2.(50. *< right) massless_body)
      false;
    inelastic_separate_test "Inelastically Separating bodies on x-axis"
      ( Body.make 2. 2. Vector2.right Vector2.left 0 0 0 "",
        Body.make 2. 2. Vector2.left Vector2.right 0 0 0 "" )
      ( Body.make 2. 2. Vector2.(right |> scale 2.) Vector2.zero 0 0 0 "",
        Body.make 2. 2. Vector2.(left |> scale 2.) Vector2.zero 0 0 0 "" )
      false;
    inelastic_separate_test "Inelastically Separating bodies on y-axis"
      ( Body.make 2. 2. Vector2.up Vector2.down 0 0 0 "",
        Body.make 2. 2. Vector2.down Vector2.up 0 0 0 "" )
      ( Body.make 2. 2. Vector2.(up |> scale 2.) Vector2.zero 0 0 0 "",
        Body.make 2. 2. Vector2.(down |> scale 2.) Vector2.zero 0 0 0 "" )
      false;
    inelastic_separate_test "Test that inelastic separation raises ZeroMass"
      (massless_body, Body.move_to Vector2.right massless_body)
      (massless_body, massless_body)
      true;
    inelastic_separate_test
      "Inelastic separation should do nothing if there is no collision"
      (unit_body, Body.move_to Vector2.(50. *< right) motion_body)
      (unit_body, Body.move_to Vector2.(50. *< right) motion_body)
      false;
    inelastic_separate_test
      "Inelastic separation should do nothing on non colliding massless bodies"
      (massless_body, Body.move_to Vector2.(90. *< right) massless_body)
      (massless_body, Body.move_to Vector2.(90. *< right) massless_body)
      false;
    body_distance_test "The distance between a body at <1, 0> and <0, 0> is 1"
      Body.(unit_body |> move_to Vector2.right)
      unit_body 1.;
    body_distance_test "The distance between a body at <0, 0> and <0, 1> is 1"
      unit_body
      Body.(unit_body |> move_to Vector2.up)
      1.;
    body_distance_test "The distance between a body and itself is 0" unit_body
      unit_body 0.;
    body_distance_test
      "The distance from a body at <0, 0> to a body at <1, 1> is sqrt 2"
      Body.(unit_body |> move_to Vector2.(make 1. 1.))
      unit_body (sqrt 2.);
    body_name_test "Testing setting a name to the empty string"
      Body.(set_name "" unit_body)
      "";
    body_name_test "Testing setting a name to a longer string"
      Body.(set_name "ahksdflkajdshfalsdkfjhalkjfdhla" unit_body)
      "ahksdflkajdshfalsdkfjhalkjfdhla";
    body_position_test "Test that unit is at <0, 0>" unit_body Vector2.zero;
    body_position_test "Test that a moved body is where it was moved to"
      Body.(unit_body |> move_to Vector2.right)
      Vector2.right;
    body_position_test "Test negative positions"
      Body.(move_to Vector2.down unit_body)
      Vector2.down;
    body_position_test "Test large positions"
      Body.(move_to Vector2.(make 100000. ~-.12312.) unit_body)
      Vector2.(make 100000. ~-.12312.);
    body_z_pos_test "Test if unit body is at z-pos 0" unit_body 0.;
    body_z_pos_test "Test moving the unit body to 0"
      Body.(set_z unit_body 0.)
      0.;
    body_z_pos_test "Test moving the unit body to 1"
      Body.(set_z unit_body 1.)
      1.;
    body_z_pos_test "Test moving the unit body to -1"
      Body.(set_z unit_body ~-.1.)
      ~-.1.;
    body_z_pos_test "Test moving the unit body to a large positive"
      Body.(set_z unit_body 3456236.)
      3456236.;
    body_z_pos_test "Test moving the unit body to a large negative"
      Body.(set_z unit_body ~-.3942857.)
      ~-.3942857.;
    body_velocity_test "Test that unit has no velocity" unit_body Vector2.zero;
    body_velocity_test "Test that motion has velocity" motion_body Vector2.up;
    body_velocity_test "Test setting the velocity to be zero"
      Body.(set_velocity Vector2.zero motion_body)
      Vector2.zero;
    body_velocity_test "Test setting velocity to be right"
      Body.(set_velocity Vector2.right unit_body)
      Vector2.right;
    body_velocity_test "Test setting velocity to be down"
      Body.(set_velocity Vector2.down motion_body)
      Vector2.down;
    body_velocity_test "Test setting velocity to be left"
      Body.(set_velocity Vector2.left motion_body)
      Vector2.left;
    body_velocity_test "Test setting velocity to be up"
      Body.(set_velocity Vector2.up unit_body)
      Vector2.up;
    body_collides_test "Bodies at the same position don't collide" unit_body
      motion_body false;
    body_collides_test "A body with radius 1 at <3,0> doesn't collide with unit"
      unit_body
      Vector2.(Body.make 1. 1. (3. *< right) zero 1 1 1 "")
      false;
    body_collides_test "Test overlapping bodies"
      Body.(unit_body |> move_to Vector2.left)
      motion_body true;
  ]

let eight_ball = Body.make 0.057 0.16 Vector2.right Vector2.zero 0 0 0 "e"

let atlas_stone =
  Body.make 0.5 (8. |> Float.pow 10.) Vector2.zero Vector2.zero 0 0 0 "a"

let gravity_for_time_test name bodies body time expected : test =
  name >:: fun _ ->
  assert_equal expected
    (Body.gravity_for_time bodies body time)
    ~cmp:body_equal ~printer:string_of_body

let gravity_tests =
  [
    gravity_for_time_test
      "Fg on eightball by atlas stone; r = 1 m; v = 0 for both; t= 5s"
      [ eight_ball; atlas_stone ]
      eight_ball 5.
      (Body.make 0.057 0.16 (Vector2.make 0.83315 0.)
         (Vector2.make (-0.03337) 0.)
         0 0 0 "e");
    gravity_for_time_test
      "Fg on eightball by atlas stone; r = 1 m; v_initial_eightball = (0,1); \
       t= 5s"
      [ eight_ball |> Body.set_velocity (Vector2.make 0. 1.); atlas_stone ]
      (eight_ball |> Body.set_velocity (Vector2.make 0. 1.))
      5.
      (Body.make 0.057 0.16 (Vector2.make 0.83315 5.)
         (Vector2.make (-0.03337) 1.)
         0 0 0 "e");
    gravity_for_time_test
      "Fg on eightball by atlas stone; r = 1 m; v_initial_eightball = (0,.1); \
       t= 20s"
      [ eight_ball |> Body.set_velocity (Vector2.make 0. 0.1); atlas_stone ]
      (eight_ball |> Body.set_velocity (Vector2.make 0. 0.1))
      20.
      (Body.make 0.057 0.16
         (Vector2.make (-1.6696) 2.)
         (Vector2.make (-0.13348) 0.1)
         0 0 0 "e");
  ]

let colliding_eight_ball =
  Body.make 0.057 0.16 Vector2.right Vector2.zero 0 0 0 "e"

let colliding_atlas_stone =
  Body.make 1. (8. |> Float.pow 10.) Vector2.zero Vector2.zero 0 0 0 ""

let colliders_test name bodies expected : test =
  name >:: fun _ ->
  assert_equal expected (Body.colliders bodies) ~cmp:body_tuple_list_equal
    ~printer:string_of_tuple_body_list

let colliders_tests =
  [
    colliders_test "2 bodies: colliding_eight_ball and colliding_atlas_stone"
      [ colliding_eight_ball; colliding_atlas_stone ]
      [ (colliding_eight_ball, colliding_atlas_stone) ];
  ]

let state_test name state expected : test =
  name >:: fun _ ->
  assert_equal expected (state |> State.bodies)
    ~cmp:(fun x y ->
      List.equal body_equal (List.sort compare x) (List.sort compare y))
    ~printer:(fun x ->
      "[" ^ String.concat ", " (List.map string_of_body x) ^ "]")

let main_state = State.(init_state [ unit_body; motion_body ])

let state_tests =
  [
    state_test "Test creating a state" main_state [ motion_body; unit_body ];
    state_test "Test creating json from a state"
      State.(
        main_state |> to_json |> Statebuilder.from_json |> Statebuilder.to_state)
      [ motion_body; unit_body ];
    state_test "Test creating state with duplicate bodies"
      State.(init_state [ unit_body; unit_body ])
      [ unit_body; unit_body ];
  ]

let statebuilders_test name statebuilder expected : test =
  state_test name (statebuilder |> Statebuilder.to_state) expected

let click_test name statebuilder click expected : test =
  name >:: fun _ ->
  assert_equal expected
    Statebuilder.(statebuilder |> register_click click)
    ~cmp:(Option.equal body_equal)
    ~printer:(fun x ->
      match x with
      | None -> "None"
      | Some x -> string_of_body x)

let statebuilders_tests =
  [
    statebuilders_test "Testing adding a body"
      Statebuilder.(empty |> add_body unit_body)
      [ unit_body ];
    statebuilders_test "Testing removing a body"
      Statebuilder.(empty |> add_body unit_body |> delete_body unit_body)
      [];
    statebuilders_test "Testing changing a body velocity"
      Statebuilder.(
        empty |> add_body unit_body |> set_velocity unit_body Vector2.up)
      [ (motion_body |> Body.(set_name (name unit_body))) ];
    statebuilders_test "Testing changing a body color"
      Statebuilder.(empty |> add_body unit_body |> set_color unit_body 1 1 1)
      [ Body.make 1. 1. Vector2.zero Vector2.zero 1 1 1 "u" ];
    statebuilders_test "Testing changing a body radius"
      Statebuilder.(empty |> add_body unit_body |> set_radius unit_body 2.)
      [ Body.make 2. 1. Vector2.zero Vector2.zero 0 0 0 "u" ];
    statebuilders_test "Testing changing a body mass"
      Statebuilder.(empty |> add_body unit_body |> set_mass unit_body 2.)
      [ Body.make 1. 2. Vector2.zero Vector2.zero 0 0 0 "u" ];
    statebuilders_test "Testing changing a body position"
      Statebuilder.(
        empty |> add_body unit_body |> set_position unit_body Vector2.up)
      [ Body.make 1. 1. Vector2.up Vector2.zero 0 0 0 "u" ];
    click_test "Test a successful click"
      Statebuilder.(empty |> add_body unit_body)
      Vector2.(0.5 *< right)
      (Some unit_body);
    click_test "Test a failed click"
      Statebuilder.(empty |> add_body unit_body)
      Vector2.(2. *< right)
      None;
    statebuilders_test "Test that bodies with negative mass will be updated"
      Statebuilder.(
        empty |> add_body Vector2.(Body.make 1. ~-.1. zero zero 1 1 1 ""))
      [ Vector2.(Body.make 1. 0.01 zero zero 1 1 1 "") ];
    statebuilders_test "Test that bodies with negative radius will be updated"
      Statebuilder.(
        empty |> add_body Vector2.(Body.make ~-.1. 1. zero zero 1 1 1 ""))
      [ Vector2.(Body.make 0.01 1. zero zero 1 1 1 "") ];
    statebuilders_test "Test that bodies with negative mass and radius"
      Statebuilder.(
        empty |> add_body Vector2.(Body.make ~-.1. ~-.1. zero zero 1 1 1 ""))
      [ Vector2.(Body.make 0.01 0.01 zero zero 1 1 1 "") ];
  ]

(* Tests that tilt correctly changes the visual positions and radius of bodies
   for the given angle*)
let tilt_test (name : string) (state : State.t) (angle : float)
    (expected : Body.t list) : test =
  name >:: fun _ ->
  assert_equal expected
    (State.tilt state angle |> State.bodies)
    ~cmp:(fun x y ->
      List.equal body_equal (List.sort compare x) (List.sort compare y))
    ~printer:(fun x ->
      "[" ^ String.concat ", " (List.map string_of_body x) ^ "]")

let tilt_tests =
  [
    tilt_test "Tilt of pi/2 radians; scene of just sun"
      (State.init_state [ unit_body_at_center ])
      (Float.pi /. 2.) [ unit_body_at_center ];
    tilt_test "Tilt of pi radians; scene of just sun"
      (State.init_state [ unit_body_at_center ])
      Float.pi [ unit_body_at_center ];
  ]

let suite =
  "test suite for SolarSystem"
  >::: List.flatten
         [
           vector2_tests;
           body_tests;
           gravity_tests;
           colliders_tests;
           state_tests;
           statebuilders_tests;
           tilt_tests;
         ]

let _ = run_test_tt_main suite
