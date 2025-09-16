open Eio.Std
module Vkt = Vk.Types
module A = Vulkan.A

type t = {
  buffer : Vkt.Buffer.t;
  indices_offset : Vkt.Device_size.t;
  index_count : int;
}

let allocate ~sw ~device { Obj_format.vertices; indices } =
  let indices =
    indices
    |> List.map Unsigned.UInt16.of_int
    |> A.of_list Ctypes.uint16_t
  in
  let size = A.sizeof vertices + A.sizeof indices in
  let buffer =
    Vulkan.Buffer.create ~sw device size
      ~usage:Vkt.Buffer_usage_flags.(vertex_buffer + index_buffer)
      ~properties:Vkt.Memory_property_flags.(host_visible + host_coherent)
  in
  Switch.run (fun sw ->
      let mapped = Vulkan.Buffer.map ~sw buffer in
      let vmapped, imapped = A.split mapped (A.sizeof vertices) in
      Memcpy.copy vertices ~dst:vmapped;
      Memcpy.copy indices ~dst:imapped;
    );
  {
    buffer = buffer.buffer;
    indices_offset = Vkt.Device_size.of_int (A.sizeof vertices);
    index_count = A.length indices;
  }

let record t command_buffer =
  let { buffer; indices_offset; index_count } = t in
  let offsets = A.of_list Vkt.Device_size.ctype [Vkt.Device_size.zero] in
  let buffers = A.of_list Vkt.Buffer.ctype_opt [Some buffer] in
  Vulkan.Cmd.bind_vertex_buffers command_buffer buffers ~first_binding:0 ~offsets;
  Vulkan.Cmd.bind_index_buffer command_buffer buffer indices_offset Uint16;
  Vulkan.Cmd.draw_indexed command_buffer
    ~first_index:0 ~index_count
    ~first_instance:0 ~instance_count:1
    ~vertex_offset:0
