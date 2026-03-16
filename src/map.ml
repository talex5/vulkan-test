module Vkt = Vk.Types
module Vec3 = Vulkan.Vec3

let width = 512
let height = 512
let size = (width, height)

let pad_x, pad_y = (0., 0.)
let pad_size = 10.
let pad_elevation = 1.

let rock_line = 8.5             (* Rocks at the top of mountains *)
let max_elevation = 9.          (* Invert heights above this (volcano effect) *)
let lava_line = 9.8             (* Lava inside volcanoes *)

let in_pad x y =
  x >= pad_x && x <= pad_x +. pad_size &&
  y >= pad_y && y <= pad_y +. pad_size

let height_map = Bigarray.(Array2.create Float32 C_layout width height)

(* OCaml's built-in mod function has unhelpful behaviour with negative numbers. *)
let (mod) a b =
  let x = a mod b in
  if x < 0 then x + b
  else x

module Bigarray = struct
  module Array2 = struct
    let get t x y = t.{x mod width, y mod height}
    let set t x y v = t.{x mod width, y mod height} <- v
  end
end

(* To avoid having to wrap the height map, starts making the land go down this far
   from the map edges. *)
let edge_water_width = 30

let clamp ~min ~max v =
  if v < min then min
  else if v > max then max
  else v

let sea_level = 0.2

let lerp a b f = a +. f *. (b -. a)

(* Generate the logical heights.
   For sea, this is the height of the sea-bed.
   For volcanoes, this is before inverting. *)
let () =
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let v =
        let fx = float x in
        let fy = float y in
        if in_pad fx fy then pad_elevation
        else (
          let v =
            15.0 *.
            Perlin.perlin2d (fx, fy)
              ~freq:(1. /. 32.0)
              ~depth:3
          in
          let to_edge =
            min edge_water_width @@
            min
              (min x (width - x))
              (min y (height - y))
          in
          lerp (-1.) v (float to_edge /. float edge_water_width)
        )
      in
      (* Don't waste too much of our 8 bits of height on underwater.
         We only draw deep vs not-deep anyway. *)
      let v = if v < sea_level then sea_level -. (sea_level -. v) /. 10. else v in
      height_map.{x, y} <- clamp ~min:0.0 ~max:10.0 v;
    done
  done

(* Apply "volcano" effect to stop things getting too high. *)
let elevation x y =
  let v = height_map.{x, y} in
  let excess_height = v -. max_elevation in
  if excess_height > 0. then v -. 2. *. excess_height else v

let create_heights ~sw ~command_pool ~device =
  Texture.init ~sw ~device ~command_pool R8_unorm (width, height) @@ fun data ->
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let v = elevation x y in
      data.{y, x} <- truncate (25.5 *. v);
    done
  done

(* The sun is straight up. *)
let sun = Vec3.v 0. 0. 1.

(* Get the unit normal vector for the tile whose SW corner is (x, y). *)
let normal x y =
  let a = elevation x y in
  let b = elevation (x + 1) y in
  let c = elevation x (y + 1) in
  let ab = b -. a in
  let ac = c -. a in
  Vec3.(norm (v (-.ab) (-.ac) 1.0))

(* Colours *)
let forest = Vec3.v 0.0 0.4 0.0
let grass = Vec3.v 0.2 0.6 0.1
let farm = Vec3.v 0.42 0.33 0.01
let rock = Vec3.v 0.4 0.5 0.4
let lava = Vec3.v 0.4 0.0 0.0

let generate_tile_colour x y =
  let n = normal x y in
  let n_dot_sun = Vec3.dot n sun in
  let typ =
    Perlin.perlin2d (float x, float y)
      ~freq:(1. /. 5.0)
      ~depth:2
  in
  let height =
    let v = height_map.{x, y} in
    if v >= lava_line then `Mountain_hollow
    else if v >= rock_line then `Mountain_top
    else `Hills
  in
  let c =
    match height with
    | `Mountain_hollow -> lava
    | `Mountain_top -> rock
    | `Hills ->
      if n_dot_sun > 0.3 && typ > 0.3 then farm
      else if typ > 0.5 then grass else forest
  in
  let c =
    Vec3.v
      (c.x +. Random.float 0.2)
      (c.y +. Random.float 0.2)
      (c.z +. Random.float 0.2)
  in
  let intensity = 0.5 +. 1.0 *. n_dot_sun in
  Vec3.scale intensity c

let pad_color x y =
  if (x lxor y) land 1 <> 0 then Vec3.v 0.8 0.8 0.8
  else Vec3.v 0.7 0.7 0.7

let tile_colour x y =
  if in_pad (float x +. 0.5) (float y +. 0.5) then pad_color x y
  else generate_tile_colour x y

let create_tiles ~sw ~command_pool ~device =
  Texture.init ~sw ~device ~command_pool B8g8r8a8_srgb (width, height) @@ fun data ->
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let c = tile_colour x y in
      let to_int x = min 255 (truncate (x *. 255.)) in
      data.{y, x} <-
        Int32.of_int @@
        to_int c.z +
        (to_int c.y lsl 8) +
        (to_int c.x lsl 16)
    done
  done

let tile_type x y =
  let v = height_map.{truncate x, truncate y} in
  if v <= sea_level then `Sea
  else if v > lava_line then `Lava
  else `Hill

let random_start_location () =
  (* Pick a point not too near the centre, then translate to be away from the pad instead. *)
  let cx, cy = width / 2, height / 2 in
  let dist a b = abs (a - b) in
  let x, y = Random.int width, Random.int height in
  let x = if dist x cx < 64 && dist y cy < 64 then x + cx else x in
  ((x + cx) mod width,
   (y + cy) mod height)
