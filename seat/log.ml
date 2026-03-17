let src = Logs.Src.create "logind" ~doc:"libinput logind client"
include (val Logs.src_log src : Logs.LOG)
