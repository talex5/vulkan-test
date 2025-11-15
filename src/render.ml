open Eio.Std
module Vkt = Vk.Types

type t = {
  device : Vulkan.Device.t;
  pipeline : Pipeline.t;
  surface : Surface.t;
  redraw_needed : Eio.Condition.t;
  mutable frame : int;
}

let create_depth_buffer ~sw t (width, height) =
  let device = t.device in
  let format = Vkt.Format.D32_sfloat in
  let img =
    Vulkan.Image.create ~sw device
      ~format
      ~extent:(Vkt.Extent_3d.make ~width ~height ~depth:1)
      ~flags:Vkt.Image_create_flags.empty
      ~tiling:Optimal
      ~usage:Vkt.Image_usage_flags.depth_stencil_attachment
      ~sharing_mode:Exclusive
      ~initial_layout:Undefined
  in
  let _image_memory : Vkt.Device_memory.t =
    Vulkan.Image.allocate_image_memory ~sw ~device img
      ~properties:Vkt.Memory_property_flags.device_local
  in
  Vulkan.Image.create_view ~sw ~format ~device img
    ~aspect_mask:Vkt.Image_aspect_flags.depth

let record_commands t job (framebuffer : Surface.framebuffer) =
  let { Duo.input; command_buffer } = job in
  Input.set input t.frame ~geometry:framebuffer.geometry;
  Vulkan.Cmd.reset command_buffer;
  Vulkan.Cmd.record command_buffer (fun () ->
      Pipeline.record t.pipeline input command_buffer framebuffer
    )

(* Should probably get this added to [Eio.Condition]. *)
let next_as_promise cond =
  let p, r = Promise.create () in
  ignore (Eio.Condition.register_immediate cond (Promise.resolve r) : Eio.Condition.request);
  p

let create_framebuffer ~sw ~depth_buffer ~on_release t geometry =
  let device = t.device in
  let image = t.surface#create_image ~sw ~device geometry in
  let memory = Vulkan.Image.allocate_image_memory ~sw ~device image
      ~handle_types:Vkt.External_memory_handle_type_flags.opaque_fd
      ~properties:Vkt.Memory_property_flags.device_local
  in
  let layout = Vulkan.Image.get_layout ~device image in
  let framebuffer =
    Vulkan.Image.create_framebuffer ~sw geometry image
      ~depth_buffer
      ~device:t.device
      ~format:t.surface#format
      ~render_pass:t.pipeline.render_pass
  in
  let render_finished = Vulkan.Semaphore.create_export ~sw device in
  let dmabuf = { Vulkan.Swap_chain.
    geometry;
    offset = Vkt.Device_size.to_int (Vkt.Subresource_layout.offset layout);
    stride = Vkt.Device_size.to_int (Vkt.Subresource_layout.row_pitch layout);
    fd = Vulkan.Image.get_memory_fd ~sw device memory;
  } in
  let rec buffer = lazy (t.surface#import_buffer ~sw ~on_release dmabuf)
  and frame = lazy Surface.{ framebuffer; buffer = Lazy.force buffer; render_finished; dma_buf_fd = dmabuf.fd; geometry } in
  Lazy.force frame

let create_swapchain ~sw t geometry =
  let depth_buffer = create_depth_buffer ~sw t geometry in
  Vulkan.Swap_chain.create (create_framebuffer ~sw ~depth_buffer t geometry)

let render_loop t duo =
  while true do
    let geometry = t.surface#geometry in
    Switch.run @@ fun sw ->
    let framebuffers = create_swapchain ~sw t geometry in
    while geometry = t.surface#geometry do
      let fb = Vulkan.Swap_chain.get_framebuffer framebuffers in
      let redraw_needed = next_as_promise t.redraw_needed in
      let job = Duo.get duo in
      record_commands t job fb;
      Duo.submit duo fb job.command_buffer;
      fb.buffer#attach;
      Promise.await redraw_needed
    done
  done

let trigger_redraw t =
  Eio.Condition.broadcast t.redraw_needed

let create ~sw ~device ~surface model =
  let pipeline, inputs = Pipeline.create ~sw ~format:surface#format ~device model in
  let command_pool = Vulkan.Cmd.create_pool ~sw device in
  let duo = Duo.make ~sw ~command_pool ~device inputs in
  let redraw_needed = Eio.Condition.create () in
  let t = { device; surface; pipeline; redraw_needed; frame = 0 } in
  Fiber.fork_daemon ~sw (fun () -> render_loop t duo);
  t
