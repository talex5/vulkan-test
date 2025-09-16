open Eio.Std

let validation = false
let validation_layers = if validation then ["VK_LAYER_KHRONOS_validation"] else []

let log fmt = Fmt.epr (fmt ^^ "@.")

let app_info = Vulkan.Instance.application_info "vulkan-test-ocaml" ~version:(1,0,0)

let main ~net ~frame_limit =
  (* The cache spawns two threads and makes traces more confusing, so disable it. *)
  Unix.putenv "MESA_SHADER_CACHE_DISABLE" "1";
  (* show_extensions (); *)
  Switch.run @@ fun sw ->
  let instance = Vulkan.Instance.create ~sw ~validation_layers app_info in
  let transport = Wayland.Unix_transport.connect ~sw ~net () in
  let window = Window.init ~sw transport in
  let physical_device = Vulkan.Instance.find_device instance window.wayland_dmabuf.main_device in
  let device = Vulkan.Device.create ~sw physical_device in
  let render = Render.create ~sw ~device ~window in
  log "Start main loop";
  while render.frame < frame_limit do
    let next_frame_due = Window.frame window in
    Render.trigger_redraw render;
    Promise.await next_frame_due;
    render.frame <- render.frame + 1
  done;
  log "Frame limit reached; exiting";
  Window.destroy window

let () =
  (* Configure logging *)
  Logs.set_level (Some Warning);
  Logs.set_reporter (Logs_fmt.reporter ~pp_header:Fmt.nop ());
  (* Start event loop *)
  Eio_main.run @@ fun env ->
  (* Parse command-line arguments *)
  let frame_limit =
    match Sys.argv with
    | [| _; x |] -> int_of_string x
    | _ -> 200
  in
  try
    main ~net:env#net ~frame_limit
  with Window.Closed ->
    log "Window closed; exiting"
