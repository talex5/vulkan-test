type buffer = < attach : unit >

type t = <
  geometry : int * int;
  import_buffer : sw:Eio.Switch.t -> on_release:(unit -> unit) -> Vulkan.Swap_chain.dmabuf -> buffer;
  frame : unit Eio.Promise.t;
>
