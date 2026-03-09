type t = {
  x : float;
  y : float;
  z : float;
}

let pp f { x; y; z } =
  Fmt.pf f "(%.2f,%.2f,%.2f)" x y z

let v x y z = { x; y; z }

let zero = v 0.0 0.0 0.0

let ( + ) a b =
  {
    x = a.x +. b.x;
    y = a.y +. b.y;
    z = a.z +. b.z;
  }

let ( - ) a b =
  {
    x = a.x -. b.x;
    y = a.y -. b.y;
    z = a.z -. b.z;
  }

let neg {x; y; z} = {
  x = -. x;
  y = -. y;
  z = -. z;
}

let cross a b =
  {
    x = (a.y *. b.z) -. (a.z *. b.y);
    y = (a.z *. b.x) -. (a.x *. b.z);
    z = (a.x *. b.y) -. (a.y *. b.x);
  }

let scale a t =
  {
    x = a *. t.x;
    y = a *. t.y;
    z = a *. t.z;
  }

let mag { x; y; z } =
  sqrt @@
  x ** 2.0 +.
  y ** 2.0 +.
  z ** 2.0

let norm t = scale (1. /. mag t) t

let dot a b =
  a.x *. b.x +.
  a.y *. b.y +.
  a.z *. b.z

let ( ** ) t a =
  {
    x = t.x ** a;
    y = t.y ** a;
    z = t.z ** a;
  }

module C = struct
  type t = [`Vec3] Ctypes.structure Ctypes.typ

  let t : t = Ctypes.structure "vec3"
  let x = Ctypes.field t "x" Ctypes.float
  let y = Ctypes.field t "y" Ctypes.float
  let z = Ctypes.field t "z" Ctypes.float
  let () = Ctypes.seal t
end

let view =
  Ctypes.view C.t
    ~read:(fun c ->
        v (Ctypes.getf c C.x)
          (Ctypes.getf c C.y)
          (Ctypes.getf c C.z)
      )
    ~write:(fun v ->
        let c = Ctypes.make C.t in
        Ctypes.setf c C.x v.x;
        Ctypes.setf c C.y v.y;
        Ctypes.setf c C.z v.z;
        c
      )

let ( *. ) = scale
