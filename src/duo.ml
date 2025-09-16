module Vkt = Vk.Types

type job = {
  input : Input.t;
  command_buffer : Vkt.Command_buffer.t;
}

type side = A | B

let other = function
  | A -> B
  | B -> A

type t = {
  a : job;
  b : job;
  mutable side : side;
}

let make ~sw ~command_pool (ia, ib) =
  let job input =
    let command_buffer = Vulkan.Cmd.allocate_buffer ~sw command_pool in
    { command_buffer; input }
  in
  { a = job ia; b = job ib; side = A }

let next t =
  t.side <- other t.side;
  match t.side with
  | A -> t.a
  | B -> t.b
