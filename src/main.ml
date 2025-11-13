open Eio.Std

let validation = false
let validation_layers = if validation then ["VK_LAYER_KHRONOS_validation"] else []

let app_info = Vulkan.Instance.application_info "vulkan-test-ocaml" ~version:(1,0,0)

let animate ~sw ~frame_limit ~instance ~device ~model surface =
  let physical_device = Vulkan.Instance.find_device instance device in
  let device = Vulkan.Device.create ~sw physical_device in
  let render = Render.create ~sw ~device ~surface model in
  while render.frame < frame_limit do
    let next_frame_due = surface#frame in
    Render.trigger_redraw render;
    Promise.await next_frame_due;
    render.frame <- render.frame + 1
  done

let main ~net ~frame_limit model =
  Switch.run @@ fun sw ->
  let instance = Vulkan.Instance.create ~sw ~validation_layers app_info in
  let animate = animate ~sw ~frame_limit ~model ~instance in
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
  (* The cache spawns two threads and makes traces more confusing, so disable it. *)
  Unix.putenv "MESA_SHADER_CACHE_DISABLE" "1";
  (* Configure logging *)
  Logs.set_level (Some Warning);
  Logs.set_reporter (Logs_fmt.reporter ~pp_header:Fmt.nop ());
  (* Start event loop *)
  Eio_main.run @@ fun env ->
  (* Parse command-line arguments *)
  let frame_limit, obj, tex =
    match Array.to_list Sys.argv with
    | [_; x; obj; tex] -> int_of_string x, obj, tex
    | [_] -> 200, "viking_room.obj", "viking_room.png"
    | _ -> failwith "usage: prog frames model.obj model.png"
  in
  let obj = Eio.Path.(with_lines (env#fs / obj)) Obj_format.parse in
  let tex = Cairo.PNG.create tex in
  try
    main ~net:env#net ~frame_limit (obj, tex)
  with Window.Closed -> ()
