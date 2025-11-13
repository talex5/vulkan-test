(* A pair of jobs. One job can be written to by the CPU while the other is processed by the GPU. *)

module Vkt = Vk.Types

type job = {
  input : Input.t;
  command_buffer : Vkt.Command_buffer.t;
}

type side = A | B

let invert = function
  | A -> B
  | B -> A

type t = {
  device : Vulkan.Device.t;
  in_flight_fence : Vkt.Fence.t;                (* Signalled when GPU finishes rendering pipeline *)
  image_available : Vkt.Semaphore.t;            (* Signalled when the compositer has finished showing the old image *)
  a : job;
  b : job;
  mutable cpu_owns : side;     (* Which job we last gave to the CPU *)
}

let make ~sw ~command_pool ~device (ia, ib) =
  let job input =
    let command_buffer = Vulkan.Cmd.allocate_buffer ~sw command_pool in
    { command_buffer; input }
  in
  {
    device;
    in_flight_fence = Vulkan.Fence.create ~sw device Vkt.Fence_create_flags.signaled;
    image_available = Vulkan.Semaphore.create ~sw device;
    a = job ia;
    b = job ib;
    cpu_owns = A;
  }

let get t =
  match t.cpu_owns with
  | A -> t.a
  | B -> t.b

(* Note: Mesa's WSI also calls set_memory_ownership when acquiring and presenting images.
   I'm not sure what that's for (it doesn't do anything on my GPU) so I skipped it. *)
let submit t (fb : _ Vulkan.Swap_chain.frame) command_buffer =
  let device = t.device in
  (* If we're still rendering the last frame, wait for that to finish.
     Needed because e.g. [image_available_semaphore] isn't per-framebuffer. *)
  Vulkan.Fence.wait device [t.in_flight_fence];
  Vulkan.Fence.reset device [t.in_flight_fence];
  (* At this point, [image_available_semaphore] is no longer in use,
     because the pipeline waited on it and reset it before finishing,
     which trigged inFlightFence. *)

  (* The CPU and GPU are now both idle, and ownership of the jobs switches here. *)
  t.cpu_owns <- invert t.cpu_owns;

  (* Get the semaphore that the compositor's render job will signal when
     it's done reading the image: *)
  Vulkan.Semaphore.import device fb.dma_buf_fd t.image_available;

  Vulkan.Cmd.submit device command_buffer
    ~wait:[t.image_available, Vkt.Pipeline_stage_flags.color_attachment_output]
    ~signal_semaphores:[fb.render_finished]
    ~fence:t.in_flight_fence;

  (* Attach [render_finished] to the dmabuf so that the compositor doesn't
     start displaying the image until the GPU has finished rendering it. *)
  Vulkan.Semaphore.export device fb.render_finished fb.dma_buf_fd
