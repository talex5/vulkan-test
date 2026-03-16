open Eio.Std

let validation =
  match Sys.getenv_opt "VK_VALIDATION" with
  | None | Some "" | Some "0" -> false
  | Some _ -> true

let validation_layers = if validation then ["VK_LAYER_KHRONOS_validation"] else []

let app_info = Vulkan.Instance.application_info "vulkan-test-ocaml" ~version:(1,0,0)

let animate ~sw ~instance ~device surface =
  let physical_device = Vulkan.Instance.find_device instance device in
  let device = Vulkan.Device.create ~sw physical_device
      ~extensions:surface#vulkan_extensions
      ~features_1_1:Vk.Types.Physical_device_vulkan_1_1_features.Fields.[shader_draw_parameters]
  in
  let render = Render.create ~sw ~device ~surface in
  let rec run_game_loop () =
    let next_frame_due = surface#frame in
    Render.trigger_redraw render;
    Promise.await next_frame_due;
    match Scene.update render.scene surface#pointer_state with
    | `Continue -> run_game_loop ()
    | `Game_over reason -> reason
  in
  run_game_loop ()

let main ~net =
  Switch.run @@ fun sw ->
  let instance = Vulkan.Instance.create ~sw ~validation_layers app_info in
  let animate = animate ~sw ~instance in
  if Sys.getenv_opt "WAYLAND_DISPLAY" <> None then (
    let transport = Wayland.Unix_transport.connect ~sw ~net () in
    let window = Window.init ~sw transport in
    let device = window.wayland_dmabuf.main_device in
    Log.info (fun f -> f "Wayland compositor main device is %a" Drm.Dev_t.pp device);
    let exit_reason = animate ~device (Window.surface window) in
    Window.destroy window;
    exit_reason
  ) else (
    Ctrl_c.install_signal_handler sw;
    let vt = Vt.init ~sw () in
    let device = Vt.device vt in
    Log.info (fun f -> f "Selected GPU device is %a" Drm.Dev_t.pp device);
    animate ~device (Vt.surface vt)
  )

let () =
  (* Configure logging *)
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ~pp_header:Fmt.nop ());
  (* Start event loop *)
  Eio_main.run @@ fun env ->
  let exit_reason =
    try main ~net:env#net with Window.Closed -> "window closed by user"
  in
  Fmt.pr "Game over (%s)@." exit_reason
