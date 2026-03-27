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
  crtc : K.Crtc.t;
  conn : K.Connector.id;
  plane : [`Plane] K.Properties.metadata;
  mutable front_fb : fb option;             (* Currently visible, last time we checked *)
  mutable enqueued_fb : fb option;          (* Enqueued to become visible (and possibly already is; confirmation pending) *)
  flip_complete : Eio.Condition.t;          (* [enqueued_fb] has become [None]. *)
  mutable next_frame_due : unit Promise.t * unit Promise.u;
}

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

let init ~sw () =
  let dev_fd = open_default_device ~sw () in
  let t =
    Eio_unix.Fd.use_exn "Vt.init" dev_fd @@ fun dev ->
    Log.info (fun f -> f "Driver version: %a" Drm.Device.Version.pp (Drm.Device.Version.get dev));
    Drm.Client_cap.(set_exn atomic) dev true;    (* Enable modern features *)
    let conn = find_connector dev in
    let crtc = get_crtc dev conn in
    let plane_props = find_primary_plane ~dev ~crtc_id:crtc.crtc_id in
    {
      dev_fd;
      crtc;
      conn = K.Connector.id conn;
      plane = plane_props.metadata;
      front_fb = None;
      enqueued_fb = None;
      flip_complete = Eio.Condition.create ();
      next_frame_due = Promise.create ();
    }
  in
  Switch.on_release sw (fun () -> restore_fb t);
  Fiber.fork_daemon ~sw (fun () -> read_events t);
  t

let device t =
  let info = Eio_unix.Fd.use_exn "Vt.device" t.dev_fd Unix.fstat in
  Drm.Dev_t.of_int64 (Int64.of_int info.st_rdev)

let geometry t =
  let mode = Option.get t.crtc.mode in
  (mode.hdisplay, mode.vdisplay)

(* Create a new Linux framebuffer from a Vulkan dmabuf.
   Typically we will have 2 or 3 framebuffers, and [attach] them in rotation. *)
let import ~sw t { Vulkan.Swap_chain.offset; stride; fd; geometry; drm_format } =
  let fb =
    Eio_unix.Fd.use_exn "Vt.import" t.dev_fd @@ fun dev ->
    let handle = Eio_unix.Fd.use_exn "Dmabuf.to_handle" fd (Drm.Dmabuf.to_handle dev) in
    Fun.protect ~finally:(fun () -> Drm.Buffer.close dev handle) @@ fun () ->
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

let surface t =
  object (_ : Surface.t)
    method format = vk_format
    method geometry = geometry t
    method frame = frame t

    method create_image ~sw ~device ~on_release geometry =
      let drm_format = Vulkan.Drm_format.v pixel_format ~modifier:Drm.Modifier.reserved in
      let image, dmabuf = Surface.create_image ~sw ~device ~format:vk_format ~drm_format geometry in
      let fb = import ~sw t dmabuf in
      let buffer =
        object
          method attach = attach ~on_release t fb
          method dma_buf_fd = dmabuf.fd
        end
      in
      (image, buffer)

    method vulkan_extensions = []
  end
