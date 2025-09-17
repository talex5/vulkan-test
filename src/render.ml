open Eio.Std
module Vkt = Vk.Types
module Swap_chain = Vulkan.Swap_chain

type t = {
  device : Vulkan.Device.t;
  format : Vkt.Format.t;
  pipeline : Pipeline.t;
  duo : Duo.t;
  redraw_needed : Eio.Condition.t;
  mutable frame : int;
  window : Window.t;
  in_flight_fence : Vkt.Fence.t;                (* Signalled when GPU finishes rendering pipeline *)
  image_available : Vkt.Semaphore.t;            (* Signalled when the compositer has finished showing the old image *)
}

let record_commands t ~geometry:(width, height) framebuffer =
  let { Duo.input; command_buffer } = Duo.next t.duo in
  Input.set input t.frame;
  Vulkan.Cmd.reset command_buffer;
  Vulkan.Cmd.record command_buffer (fun () ->
      Pipeline.record t.pipeline input command_buffer (width, height) framebuffer
    );
  command_buffer

let next_as_promise cond =
  let p, r = Promise.create () in
  ignore (Eio.Condition.register_immediate cond (Promise.resolve r) : Eio.Condition.request);
  p

let create_framebuffer t =
  Vulkan.Image.create_framebuffer ~device:t.device ~format:t.format ~render_pass:t.pipeline.render_pass

(* Note: Mesa's WSI also calls set_memory_ownership when acquiring and presenting images.
   I'm not sure what that's for (it doesn't do anything on my GPU) so I skipped it. *)
let submit_frame t (fb : Swap_chain.frame) command_buffer =
  (* If we're still rendering the last frame, wait for that to finish.
     Needed because e.g. [image_available_semaphore] isn't per-framebuffer. *)
  let device = t.device in
  Vulkan.Fence.wait device [t.in_flight_fence];
  Vulkan.Fence.reset device [t.in_flight_fence];
  (* At this point, [image_available_semaphore] is no longer in use,
     because the pipeline waited on it and reset it before finishing,
     which trigged inFlightFence. *)

  (* Get the semaphore that the compositor's render job will signal when
     it's done reading the image: *)
  Vulkan.Semaphore.import device fb.dma_buf_fd t.image_available;

  Vulkan.Cmd.submit device command_buffer
    ~wait:[t.image_available, Vkt.Pipeline_stage_flags.color_attachment_output]
    ~signal_semaphores:[fb.render_finished]
    ~fence:t.in_flight_fence;

  (* Attach [render_finished] to the dmabuf so that the compositor doesn't
     start displaying the image until the GPU has finished rendering it. *)
  Vulkan.Semaphore.export device fb.render_finished fb.dma_buf_fd;
  (* Note: draw_frame waits for the previous frame to finish, so the other input is now free *)
  Window.attach t.window ~buffer:fb.wl_buffer

let render_loop t ~make_swap_chain =
  while true do
    let geometry = Window.geometry t.window in
    Switch.run @@ fun sw ->
    let framebuffers = make_swap_chain ~sw geometry (create_framebuffer ~sw t) in
    while geometry = Window.geometry t.window do
      let fb = Swap_chain.get_framebuffer framebuffers in
      let redraw_needed = next_as_promise t.redraw_needed in
      record_commands t ~geometry fb.framebuffer |> submit_frame t fb;
      Promise.await redraw_needed
    done
  done

let trigger_redraw t =
  Eio.Condition.broadcast t.redraw_needed

let create ~sw ~device ~window =
  let format = Vkt.Format.B8g8r8a8_srgb in
  let pipeline, inputs = Pipeline.create ~sw ~format device in
  let command_pool = Vulkan.Cmd.create_pool ~sw device in
  let duo = Duo.make ~sw ~command_pool inputs in
  let redraw_needed = Eio.Condition.create () in
  let dmabuf = window.Window.wayland_dmabuf in
  let make_swap_chain = Vulkan.Swap_chain.create ~dmabuf ~device ~format in
  let t = {
    device; format; window; pipeline; duo; redraw_needed; frame = 0;
    in_flight_fence = Vulkan.Fence.create ~sw device Vkt.Fence_create_flags.signaled;
    image_available = Vulkan.Semaphore.create ~sw device;
  } in
  Fiber.fork_daemon ~sw (fun () -> render_loop t ~make_swap_chain);
  t
