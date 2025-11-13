open Eio.Std

let interrupts = ref 0

let handle_ctrl_c set_interrupted (_ : int) =
  incr interrupts;
  if !interrupts = 1 then Promise.resolve set_interrupted ()
  else (
    print_endline "Ctrl-C pressed twice; emergency exit!";
    exit 1
  )

let install_signal_handler sw =
  let interrupted, set_interrupted = Promise.create () in
  Fiber.fork_daemon ~sw (fun () -> Promise.await interrupted; raise Sys.Break);
  let _old : Sys.signal_behavior = Sys.(signal sigint) (Signal_handle (handle_ctrl_c set_interrupted)) in
  ()
