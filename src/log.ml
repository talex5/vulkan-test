let src = Logs.Src.create "vulkan_test" ~doc:"vulkan_test"
include (val Logs.src_log src : Logs.LOG)
