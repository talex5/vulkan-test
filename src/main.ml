open Eio.Std

let validation = false
let validation_layers = if validation then ["VK_LAYER_KHRONOS_validation"] else []

let app_info = Vulkan.Instance.application_info "vulkan-test-ocaml" ~version:(1,0,0)

let animate ~sw ~instance ~device surface : unit =
  let physical_device = Vulkan.Instance.find_device instance device in
  let device = Vulkan.Device.create ~sw physical_device in
  let render = Render.create ~sw ~device ~surface in
  while true do
    let next_frame_due = surface#frame in
    Render.trigger_redraw render;
    Promise.await next_frame_due;
    Ship.update render.scene.ship ~pointer:surface#pointer_state;
  done

let main ~net =
  Switch.run @@ fun sw ->
  let instance = Vulkan.Instance.create ~sw ~validation_layers app_info in
  let animate = animate ~sw ~instance in
  if Sys.getenv_opt "WAYLAND_DISPLAY" <> None then (
    let transport = Wayland.Unix_transport.connect ~sw ~net () in
    let window = Window.init ~sw transport in
    let device = window.wayland_dmabuf.main_device in
    Log.info (fun f -> f "Wayland compositor main device is %a" Drm.Dev_t.pp device);
    animate ~device (Window.surface window);
    Window.destroy window
  ) else (
    Ctrl_c.install_signal_handler sw;
    let vt = Vt.init ~sw () in
    let device = Vt.device vt in
    Log.info (fun f -> f "Selected GPU device is %a" Drm.Dev_t.pp device);
    animate ~device (Vt.surface vt)
  )

let () =
  (* Configure logging *)
  Logs.set_level (Some Warning);
  Logs.set_reporter (Logs_fmt.reporter ~pp_header:Fmt.nop ());
  (* Start event loop *)
  Eio_main.run @@ fun env ->
  try
    main ~net:env#net
  with Window.Closed -> ()
