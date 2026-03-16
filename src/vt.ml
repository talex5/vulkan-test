open Eio.Std

module Vkt = Vk.Types
module K = Drm.Kms

let vk_format = Vkt.Format.B8g8r8a8_srgb
let pixel_format = Drm.Fourcc.xr24

type fb = {
  on_release : unit -> unit;
}

type t = {
  dev_fd : Eio_unix.Fd.t;
  gbm : Gbm.Device.t;
  modifiers : Drm.Modifier.t list option;
  crtc : K.Crtc.t;
  conn : K.Connector.id;
  plane : [`Plane] K.Properties.metadata;
  mutable front_fb : fb option;             (* Currently visible, last time we checked *)
  mutable enqueued_fb : fb option;          (* Enqueued to become visible (and possibly already is; confirmation pending) *)
  flip_complete : Eio.Condition.t;          (* [enqueued_fb] has become [None]. *)
  mutable next_frame_due : unit Promise.t * unit Promise.u;
  pointer_state : Surface.pointer_state ref;
}

let geometry t =
  let mode = Option.get t.crtc.mode in
  (mode.hdisplay, mode.vdisplay)

module In = struct
  let log_level = Input.Context.Log.Priority.Info

  type nonrec t = {
    vt : t;
    count : (Input.Device.Capability.t, int) Hashtbl.t;
  }

  let useful_types = Input.Device.Capability.[Keyboard; Pointer]

  let count t cap =
    Hashtbl.find_opt t.count cap |> Option.value ~default:0

  let update_count t ev delta =
    let dev = Input.Event.get_device ev in
    useful_types |> List.iter @@ fun cap ->
    if Input.Device.has_capability dev cap then (
      Hashtbl.replace t.count cap (count t cap + delta)
    )

  (* Handle one event *)
  let handle t : Input.Event.ty -> unit = function
    | `Device_added   e -> update_count t e (+1)
    | `Device_removed e -> update_count t e (-1)
    | `Keyboard_key e when Input.Event.Keyboard.get_key e = Input.Keycode.key_esc -> raise Exit
    | `Pointer_motion e ->
      let dx = Input.Event.Pointer.get_dx e in
      let dy = Input.Event.Pointer.get_dy e in
      let state = !(t.vt.pointer_state) in
      let width, height = geometry t.vt in
      t.vt.pointer_state := {
        state with
        x = state.x +. dx /. float width;
        y = state.y +. dy /. float height;
      }
    | `Pointer_button e ->
      let b = Input.Event.Pointer.get_button e in
      if b = Input.Keycode.btn_left then (
        let thrust =
          match Input.Event.Pointer.get_button_state e with
          | `Released -> 0.0
          | `Pressed -> 1.0
        in
        t.vt.pointer_state := { !(t.vt.pointer_state) with thrust }
      )
    | `Tablet_tool_axis e ->
      let x = Input.Event.Tablet_tool.get_x_transformed ~width:1000000 e /. 1e6 in
      let y = Input.Event.Tablet_tool.get_y_transformed ~height:1000000 e /. 1e6 in
      let thrust = Input.Event.Tablet_tool.get_pressure e in
      t.vt.pointer_state := { x; y; thrust }
    | _ -> ()

  (* Handle all events in the queue *)
  let rec handle_events t ctx =
    match Input.Event.get ctx with
    | None -> ()
    | Some event ->
      handle t (Input.Event.get_type event);
      Input.Event.destroy event;  (* Optional; don't bother waiting for GC *)
      handle_events t ctx

  let create ~sw =
    let interface = Input.Interface.unix_direct in      (* todo: systemd-logind support *)
    let udev = Input.Udev.create () in
    let ctx = Input.Context.Udev.create interface udev in
    Switch.on_release sw (fun () -> Input.Context.destroy ctx);
    Input.Context.Log.set_priority ctx log_level;
    Input.Context.Udev.assign_seat ctx "seat0";
    ctx

  let run vt ctx =
    let t = { vt; count = Hashtbl.create 2 } in
    Input.Context.dispatch ctx;
    handle_events t ctx;
    (* We could continue here and let the user plug devices in later,
       but it's likely something has gone wrong and it's better to display
       an error message. *)
    if count t Pointer = 0 then failwith "No pointer devices available; giving up";
    if count t Keyboard = 0 then failwith "No keyboard devices available; giving up";
    let fd = Input.Context.get_fd ctx in
    while true do
      Eio_unix.await_readable fd;
      Input.Context.dispatch ctx;
      handle_events t ctx;
    done
end

let open_device ~sw (d : Drm.Device.Info.t) =
  match d.primary_node with
  | None -> None
  | Some primary ->
    let unix_fd = Unix.openfile primary [O_RDWR; O_CLOEXEC] 0 in
    let fd = Eio_unix.Fd.of_unix ~sw ~close_unix:true unix_fd in
    if not (Drm.Device.is_kms unix_fd) then None
    else Some fd

(* Open the first suitable device *)
let open_default_device ~sw () =
  let devices = Drm.Device.list () in
  Log.info (fun f -> f "@[<v2>Found devices:@ %a@]" (Fmt.Dump.list Drm.Device.Info.pp) devices);
  match List.find_map (open_device ~sw) devices with
  | None -> failwith "No GPU device with a primary KMS node found"
  | Some x -> x

(* Select the first connected connector *)
let find_connector dev =
  let get_connected id =
    let c = K.Connector.get dev id in
    if c.connection = Connected then Some c
    else None
  in
  let mode_res = K.Resources.get dev in
  match List.find_map get_connected mode_res.connectors with
  | None -> failwith "No connected connectors!"
  | Some c ->
    Log.info (fun f -> f "Using first connected connector: %a" K.Connector.pp_name c);
    c

(* Loop to wait for and process Flip_complete events.
   Moves [enqueued_buffer] to [front_buffer] when that happens, and notifies [flip_complete]. *)
let read_events t =
  Eio_unix.Fd.use_exn "read_events" t.dev_fd @@ fun fd ->
  let buffer = Drm.Event.create_buffer () in
  while true do
    Eio_unix.await_readable fd;
    let len = Unix.read_bigarray fd buffer 0 (Bigarray.Array1.dim buffer) in
    Log.debug (fun f -> f "Got %d bytes from DRM device" len);
    Drm.Event.parse buffer len
    |> List.iter (function
        | Drm.Event.Flip_complete _ ->
          let old_front = t.front_fb in
          t.front_fb <- t.enqueued_fb;
          t.enqueued_fb <- None;
          Option.iter (fun old -> old.on_release ()) old_front;
          Eio.Condition.broadcast t.flip_complete
        | ev ->
          Log.warn (fun f -> f "Unexpected event %a" Drm.Event.pp ev)
      )
  done

(* Find the CRTC currently being used for [c]. *)
let get_crtc dev (c : K.Connector.t) =
  let c_props = K.Connector.get_properties dev c.connector_id in
  match K.Properties.Values.get_value_exn c_props K.Connector.crtc_id with
  | None -> Fmt.failwith "Connector %a has no CRTC!" K.Connector.pp_name c
  | Some crtc_id -> K.Crtc.get dev crtc_id

let restore_fb t =
  Eio_unix.Fd.use_exn "import" t.dev_fd @@ fun fd ->
  Log.info (fun f -> f "Restore old VT mode");
  let crtc = t.crtc in
  K.Crtc.set fd crtc.crtc_id crtc.mode
    ?fb:crtc.fb_id
    ~pos:(crtc.x, crtc.y)
    ~connectors:[t.conn]

let or_fail msg = function
  | None -> failwith msg
  | Some x -> x

let find_primary_plane ~dev ~crtc_id =
  K.Plane.list dev
  |> List.find_map (fun id ->
      let ( let* ) = Option.bind in
      let props = K.Plane.get_properties dev id in
      let get k = K.Properties.Values.get_value props k in
      let* plane_crtc_id = get K.Plane.crtc_id in
      let* typ = get K.Plane.typ in
      if plane_crtc_id = Some crtc_id && typ = `Primary then Some props
      else None
    )
  |> or_fail "No suitable KMS plane found!"

(* Get the list of modifiers supported by both us and the plane.
   [None] if the device doesn't support modifiers. *)
let get_modifiers ~dev ~gbm plane_props =
  let plane_count = Gbm.Device.get_format_modifier_plane_count gbm pixel_format in
  K.Properties.Values.get_value plane_props K.Plane.in_formats
  |> Option.map (fun blob_id ->
      let modifiers =
        K.Plane.get_in_formats dev blob_id
        |> List.filter_map (fun (f, m) ->
            if f = pixel_format && plane_count m = 1 then Some m else None
          )
      in
      Log.info (fun f -> f "Filtered modifiers %a" (Fmt.Dump.list Drm.Modifier.pp) modifiers);
      if modifiers = [] then failwith "No usable modifiers!";
      modifiers
    )

let init ~sw () =
  let ctx = In.create ~sw in
  let pointer_state = ref { Surface.x = 0.5; y = 0.4; thrust = 0.0 } in
  let dev_fd = open_default_device ~sw () in
  let t =
    Eio_unix.Fd.use_exn "Vt.init" dev_fd @@ fun dev ->
    Log.info (fun f -> f "Driver version: %a" Drm.Device.Version.pp (Drm.Device.Version.get dev));
    Drm.Client_cap.(set_exn atomic) dev true;    (* Enable modern features *)
    let gbm = Gbm.Device.create dev in
    Switch.on_release sw (fun () -> Gbm.Device.destroy gbm);
    let conn = find_connector dev in
    let crtc = get_crtc dev conn in
    let plane_props = find_primary_plane ~dev ~crtc_id:crtc.crtc_id in
    let modifiers = get_modifiers ~dev ~gbm plane_props in
    {
      dev_fd;
      gbm;
      modifiers;
      crtc;
      conn = K.Connector.id conn;
      plane = plane_props.metadata;
      front_fb = None;
      enqueued_fb = None;
      flip_complete = Eio.Condition.create ();
      next_frame_due = Promise.create ();
      pointer_state;
    }
  in
  Switch.on_release sw (fun () -> restore_fb t);
  Fiber.fork_daemon ~sw (fun () -> In.run t ctx);
  Fiber.fork_daemon ~sw (fun () -> read_events t);
  t

let device t =
  let info = Eio_unix.Fd.use_exn "Vt.device" t.dev_fd Unix.fstat in
  Drm.Dev_t.of_int64 (Int64.of_int info.st_rdev)

(* Create a new Linux framebuffer from a Vulkan dmabuf.
   Typically we will have 2 or 3 framebuffers, and [attach] them in rotation. *)
let import ~sw t { Vulkan.Swap_chain.offset; stride; fd; geometry; drm_format } =
  let fb =
    Eio_unix.Fd.use_exn "Vt.import" t.dev_fd @@ fun dev ->
    let bo = Eio_unix.Fd.use_exn "Bo.import" fd (fun fd ->
        let plane = { Gbm.Bo.fd; stride; offset } in
        let modifier = drm_format.modifier in
        let width, height = geometry in
        Gbm.Bo.import t.gbm ~flags:Gbm.Bo.Flags.use_scanout (
          Fd { width; height; format = drm_format.code; modifier; planes = [plane] }
        )
      )
    in
    match bo with
    | Error e -> raise (Unix.Unix_error (e, "Gbm.Bo.import", ""))
    | Ok bo ->
      Fun.protect ~finally:(fun () -> Gbm.Bo.destroy bo) @@ fun () ->
      let handle = Gbm.Bo.get_handle bo in
      let planes = [{ K.Fb.Plane.handle; offset; pitch = stride }] in
      K.Fb.add dev ~size:geometry ~pixel_format:drm_format.code ~planes
        ?modifier:(Vulkan.Drm_format.get_modifier_opt drm_format)
  in
  Switch.on_release sw (fun () ->
      Eio_unix.Fd.use_exn "Vt.import.release" t.dev_fd @@ fun dev ->
      (* Fb.close allows [fb] to continue being displayed until a new framebuffer is installed.
         In our case, this avoids turning the screen off briefly before [restore_fb] does its job. *)
      K.Fb.close dev fb
    );
  fb

let attach ~on_release t fb =
  while t.enqueued_fb <> None do
    Log.debug (fun f -> f "Wait for previous frame to be displayed before enqueuing another");
    Eio.Condition.await_no_mutex t.flip_complete
  done;
  Log.debug (fun f -> f "Enqueue framebuffer %a" Drm.Id.pp fb);
  Eio_unix.Fd.use_exn "Vt.attach" t.dev_fd (fun dev ->
      let rq = K.Atomic_req.create () in
      let ( .%{}<- ) obj k v = K.Atomic_req.add_property rq obj k v in
      t.plane.%{ K.Plane.fb_id } <- Some fb;
      K.Atomic_req.commit dev rq ~page_flip_event:0n;
    );
  t.enqueued_fb <- Some { on_release };
  (* This is probably a good time to start rendering another frame. *)
  ignore (Promise.try_resolve (snd t.next_frame_due) () : bool)

let frame t =
  if Promise.is_resolved (fst t.next_frame_due) then (
    t.next_frame_due <- Promise.create ()
  );
  fst t.next_frame_due

let create_framebuffer_with_modifiers ~sw ~modifiers t (width, height) =
  (* In theory, GBM should create an image large enough for rendering.
     By on rpi4, it needs to be a bit larger (V3D_TFU_READAHEAD_SIZE), so add an extra row. *)
  let geometry_with_margin = (width, height + 1) in
  let flags = Gbm.Bo.Flags.(use_scanout + use_rendering) in
  let bo = Gbm.Bo.create_exn ~flags t.gbm ~format:pixel_format geometry_with_margin ~modifiers in
  let offset = Gbm.Bo.get_offset bo 0 in
  let stride = Gbm.Bo.get_stride_for_plane bo 0 in
  let modifier = Gbm.Bo.get_modifier bo in
  let plane_count = Gbm.Bo.get_plane_count bo in
  Log.info (fun f -> f "GBM selected modifier %a (%d planes)" Drm.Modifier.pp modifier plane_count);
  assert (Gbm.Bo.get_plane_count bo = 1);
  let fd = Eio_unix.Fd.of_unix ~sw ~close_unix:true (Gbm.Bo.get_fd bo) in
  Gbm.Bo.destroy bo;
  { Vulkan.Swap_chain.
    geometry = (width, height);
    offset;
    stride;
    fd;
    drm_format = Vulkan.Drm_format.v pixel_format ~modifier;
  }

let surface t =
  object (_ : Surface.t)
    method format = vk_format
    method geometry = geometry t
    method frame = frame t

    method create_image ~sw ~device ~on_release geometry =
      let image, dmabuf =
        match t.modifiers with
        | None ->
          let drm_format = Vulkan.Drm_format.v pixel_format ~modifier:Drm.Modifier.reserved in
          Surface.create_image ~sw ~device ~format:vk_format ~drm_format geometry
        | Some modifiers ->
          let mods = Vulkan.Device.get_format_modifiers device vk_format in
          Log.info (fun f -> f "Vulkan permitted modifiers: %a" (Fmt.Dump.list Drm.Modifier.pp) mods);
          let handle_type = Vkt.External_memory_handle_type_flags.dma_buf_ext in
          let dmabuf = create_framebuffer_with_modifiers ~sw ~modifiers t geometry in
          let image =
            let (width, height) = geometry in
            let drm_info =
              Vulkan.Drm_format.get_modifier_opt dmabuf.drm_format
              |> Option.map (fun m ->
                let plane =
                  Vkt.Subresource_layout.make
                    ~offset:(Vkt.Device_size.of_int dmabuf.offset)
                    ~row_pitch:(Vkt.Device_size.of_int dmabuf.stride)
                    ~size:Vkt.Device_size.zero (* Vulkan spec says this is ignored *)
                    ~array_pitch:Vkt.Device_size.zero
                    ~depth_pitch:Vkt.Device_size.zero
                in
                (m, [plane])
              ) in
            (* The spec says "If the pNext chain includes a VkExternalMemoryImageCreateInfo ... whose handleTypes
               member is not 0, it is as if VK_IMAGE_CREATE_ALIAS_BIT is set."
               So we don't need to set [alias] here, and in fact doing so triggers a validation error on i915. *)
            Vulkan.Image.create ~sw device
              ~format:vk_format
              ~extent:(Vkt.Extent_3d.make ~width ~height ~depth:1)
              ~usage:Vkt.Image_usage_flags.color_attachment
              ~sharing_mode:Exclusive
              ~initial_layout:Undefined
              ~flags:Vkt.Image_create_flags.empty
              ~tiling:Drm_format_modifier_ext
              ~handle_types:handle_type
              ?drm_info
          in
          Vulkan.Image.import_memory_fd ~sw ~device image dmabuf.fd;
          image, dmabuf
      in
      let fb = import ~sw t dmabuf in
      let buffer =
        object
          method attach = attach ~on_release t fb
          method dma_buf_fd = dmabuf.fd
        end
      in
      (image, buffer)

    method vulkan_extensions =
      if t.modifiers = None then []
      else ["VK_EXT_image_drm_format_modifier"]

    method pointer_state = !(t.pointer_state)
  end
