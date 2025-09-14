let src = Logs.Src.create "vulkan" ~doc:"Vulkan Olivine wrappers"
include (val Logs.src_log src : Logs.LOG)
