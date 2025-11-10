open Common

let int_of_unix (fd : Unix.file_descr) : int = Obj.magic fd
let unix_of_int (fd : int) : Unix.file_descr = assert (fd >= 0); Obj.magic fd

let create ?handle_types ~sw device =
  let export_info handle_types =
    let info = Vkt.Export_semaphore_create_info.make ~handle_types () in
    ext (Vkt.Export_semaphore_create_info.addr info)
  in
  let next = Option.map export_info handle_types in
  let create_info = Vkt.Semaphore_create_info.make ?next () in
  let t = Vkc.create_semaphore ~device:(Device.dev device) ~create_info () <?> "create_semaphore" in
  Switch.on_release sw (fun () -> Device.wait_idle device; Vkc.destroy_semaphore (Device.dev device) (Some t) None);
  t

let create_export = create ~handle_types:Vkt.External_semaphore_handle_type_flags.sync_fd

let import device fd semaphore =
  Eio_unix.Fd.use_exn "Semaphore.import" fd @@ fun fd ->
  let fd = Drm.Dmabuf.export_sync_file fd `RW in
  let module E = (val device.Device.ext) in
  E.import_semaphore_fd_khr @@
  Vkt.Import_semaphore_fd_info_khr.make ()
    ~semaphore
    ~flags:Vkt.Semaphore_import_flags.temporary
    ~handle_type:Vkt.External_semaphore_handle_type_flags.sync_fd
    ~fd:(int_of_unix fd)         (* Takes ownership of the FD *)

let export device semaphore fd =
  let get_fd_info = Vkt.Semaphore_get_fd_info_khr.make ()
      ~semaphore
      ~handle_type:Vkt.External_semaphore_handle_type_flags.sync_fd
  in
  let module E = (val device.Device.ext) in
  let sync_file_fd = unix_of_int (E.get_semaphore_fd_khr get_fd_info) in
  Fun.protect ~finally:(fun () -> Unix.close sync_file_fd) @@ fun () ->
  Eio_unix.Fd.use_exn "import_sync_file" fd (fun fd -> Drm.Dmabuf.import_sync_file fd ~sync_file_fd `RW)
