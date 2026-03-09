open Common

type t = float array

let i (row, col) = col + row * 4

(* Hack to let us use [t.{x, y}] syntax. *)
module Bigarray = struct
  module Array2 = struct
    let get t r c = Array.get t (i (r, c))
  end
end

let init fn = Array.init 16 (fun i -> fn (i / 4, i mod 4))

let id = init (fun (r, c) -> if (r = c) then 1.0 else 0.0)

let rot a1 a2 th =
  init (fun (r, c) ->
      if c = a1 then (
        if r = a1 then cos th
        else if r = a2 then sin th
        else 0.0
      ) else if c = a2 then (
        if r = a1 then -.(sin th)
        else if r = a2 then cos th
        else 0.0
      ) else (
        if r = c then 1.0
        else 0.0
      )
    )

let rot_x = rot 1 2
let rot_y = rot 0 2
let rot_z = rot 0 1

let scale f =
  init (fun (r, c) -> if r = c then ( if r = 3 then 1.0 else f) else 0.0)

let translate {Vec3.x; y; z} =
  init (fun (r, c) ->
      match c, r with
      | 3, 0 -> x
      | 3, 1 -> y
      | 3, 2 -> z
      | _ when r = c -> 1.0
      | _ -> 0.0
    )

let write t c =
  for row = 0 to 3 do
    for col = 0 to 3 do
      A.set c (col * 4 + row) t.{row, col}
    done
  done

let scale_axis a f =
  init (fun (r, c) ->
      if r = c then (
        if r = a then f else 1.
      ) else 0.
    )

let scale_x = scale_axis 0
let scale_y = scale_axis 1
let scale_z = scale_axis 2

let pp f t =
  Fmt.pf f "@[<v>";
  for row = 0 to 3 do
    let cell f col = Fmt.pf f "% 3.1f" t.{row, col} in
    Fmt.pf f "| %a %a %a %a |@,"
      cell 0
      cell 1
      cell 2
      cell 3
  done;
  Fmt.pf f "@]"

let perspective_projection ~fov_y ~aspect ~z_near ~z_far =
  let scale_y = 1. /. tan (fov_y /. 2.) in
  let scale_x = scale_y /. aspect in
  let scale_z = (z_far +. z_near) /. (z_near -. z_far) in
  let bias_z = (2. *. z_far *. z_near) /. (z_near -. z_far) in
  init (fun (r, c) ->
      match r, c with
      | 0, 0 -> scale_x         (* Normalise x for FOV *)
      | 1, 1 -> scale_y         (* Normalise y for FOV *)
      | 2, 2 -> scale_z         (* Normalise z (for clipping) *)
      | 2, 3 -> bias_z          
      | 3, 2 -> -1.             (* Make far-away things smaller *)
      | _ -> 0.0
    )

let ( * ) a b =
  init (fun (r, c) ->
      let p x = a.{r, x} *. b.{x, c} in
      p 0 +. p 1 +. p 2 +. p 3
    )

let ( + ) a b =
  init (fun (r, c) -> a.{r, c} +. b.{r, c})
