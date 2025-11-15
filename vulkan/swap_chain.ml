type 'a t = {
  queue : 'a Queue.t;
  create_frame : on_release:(unit -> unit) -> 'a;
}

type dmabuf = {
  geometry : int * int;
  offset : int;
  stride : int;
  fd : Eio_unix.Fd.t;
}

let create create_frame =
  let queue = Queue.create () in
  { queue; create_frame }

let get_framebuffer t =
  try Queue.take t.queue
  with Queue.Empty ->
    let rec frame = lazy (t.create_frame ~on_release)
    and on_release () = Queue.add (Lazy.force frame) t.queue in
    Lazy.force frame
