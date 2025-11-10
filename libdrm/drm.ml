module Format = Drm_format

module Dev_t = Dev_t

let int_of_unix (fd : Unix.file_descr) : int = Obj.magic fd
let unix_of_int (fd : int) : Unix.file_descr = assert (fd >= 0); Obj.magic fd

module Dma_buf_export_sync_file = struct
  type mark
  type t = mark Ctypes.structure
  let ctype : t Ctypes.typ = Ctypes.structure "Dma_buf_export_sync_file"
  let flags = Ctypes.field ctype "flags" Ctypes.uint32_t
  let fd = Ctypes.field ctype "fd" Ctypes.int32_t
  let () = Ctypes.seal ctype

  let make `RW =
    let export = Ctypes.make ctype in
    Ctypes.setf export flags (Unsigned.UInt32.of_int Config.dma_buf_sync_rw);
    Ctypes.setf export fd (-1l);
    export
end

module Dma_buf_import_sync_file = struct
  type mark
  type t = mark Ctypes.structure
  let ctype : t Ctypes.typ = Ctypes.structure "Dma_buf_import_sync_file"
  let flags = Ctypes.field ctype "flags" Ctypes.uint32_t
  let fd = Ctypes.field ctype "fd" Ctypes.int32_t
  let () = Ctypes.seal ctype

  let make x `RW =
    let export = Ctypes.make ctype in
    Ctypes.setf export flags (Unsigned.UInt32.of_int Config.dma_buf_sync_rw);
    Ctypes.setf export fd (Int32.of_int (int_of_unix x));
    export
end

let fd =
  Ctypes.view ~read:unix_of_int ~write:int_of_unix Ctypes.int

let drm_ioctl_raw =
  Foreign.foreign ~check_errno:true "drmIoctl" Ctypes.(fd @-> ulong @-> ptr void @-> returning int)

let drm_ioctl fd op arg = drm_ioctl_raw fd (Unsigned.ULong.of_int op) (Ctypes.to_voidp (Ctypes.addr arg))

let export_sync_file fd flags =
  let export = Dma_buf_export_sync_file.make flags in
  let ret = drm_ioctl fd Config.dma_buf_ioctl_export_sync_file export in
  assert (ret = 0);
  let fd = Ctypes.getf export Dma_buf_export_sync_file.fd in
  assert (fd >= 0l);
  Int32.to_int fd |> unix_of_int

let import_sync_file fd ~sync_file_fd flags =
  let import = Dma_buf_import_sync_file.make sync_file_fd flags in
  let ret = drm_ioctl fd Config.dma_buf_ioctl_import_sync_file import in
  assert (ret = 0)
