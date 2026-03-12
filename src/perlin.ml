(* Based on https://gpfault.net/posts/perlin-noise.txt.html *)

let prng =
  let p = Array.init 256 Fun.id in
  Array.shuffle ~rand:Random.int p;
  p

let fade x = x *. x *. x *. (x *. (x *. 6. -. 15.) +. 10.)

let lerp a b f = a +. f *. (b -. a)

module Vec2 = struct
  type t = {
    x : float;
    y : float;
  }

  let map f { x; y } = { x = f x; y = f y }
  let map2 f a b = { x = f a.x b.x; y = f a.y b.y }
  let ( * ) c = map (( *. ) c)
  let ( + ) = map2 ( +. )
  let ( - ) = map2 ( -. )

  let ( *. ) a b =
    (a.x *. b.x) +.
    (a.y *. b.y)

  let to_int t = truncate t.x, truncate t.y
  let floor = map floor

  let zero = { x = 0.0; y = 0.0 }
  let x = { x = 1.0; y = 0.0 }
  let y = { x = 0.0; y = 1.0 }
end

(* Random fixed value for each point on a 256x256 grid. *)
let noise2 t =
  let x, y = Vec2.to_int t in
  let h1 = prng.(x land 0xff) in
  prng.((h1 + y) land 0xff)

let gradients = Vec2.[|
    (x + y);
    (x - y);
    (y - x);
    (zero - x - y);
|]

let grad p = gradients.(noise2 p land 3)

(* Interpolate between the fixed values. *)
let noise2d p =
  (* Get the four nearest fixed points *)
  let p0 = Vec2.floor p in
  let p1 = Vec2.(p0 + x) in
  let p2 = Vec2.(p0 + y) in
  let p3 = Vec2.(p2 + x) in
  (* Get the gradients at those points *)
  let g0 = grad p0 in
  let g1 = grad p1 in
  let g2 = grad p2 in
  let g3 = grad p3 in
  (* Calculate how far we are from p0 and smooth it *)
  let xf = fade @@ p.x -. p0.x in
  let yf = fade @@ p.y -. p0.y in
  (* Calculate the effect of each fixed point and interpolate between them *)
  let p0p1 = lerp
    Vec2.(g0 *. (p - p0))
    Vec2.(g1 *. (p - p1))
    xf
  in
  let p2p3 = lerp
    Vec2.(g2 *. (p - p2))
    Vec2.(g3 *. (p - p3))
    xf
  in
  lerp p0p1 p2p3 yf

(* Combine [noise] [depth] times. *)
let perlin2d ~freq ~depth (x, y) =
  let p = { Vec2.x; y } in
  let rec aux ~amp ~div ~acc = function
    | 0 -> acc /. div
    | depth ->
      aux (depth - 1)
        ~amp:(amp /. 2.)
        ~div:(div +. amp)
        ~acc:(acc +. (noise2d Vec2.(freq * p) *. amp))
  in
  aux ~amp:1.0 ~acc:0.0 ~div:0.0 depth
