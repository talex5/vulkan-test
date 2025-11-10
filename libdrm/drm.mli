val export_sync_file : Unix.file_descr -> [`RW] -> Unix.file_descr
val import_sync_file : Unix.file_descr -> sync_file_fd:Unix.file_descr -> [`RW] -> unit

module Format = Drm_format
module Dev_t = Dev_t
