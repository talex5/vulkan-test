module Mgr = Login1.Login1_client.Org_freedesktop_login1_Manager
module Session = Login1.Login1_client.Org_freedesktop_login1_Session

let (let*) = Lwt.bind

type t = {
  session : OBus_proxy.t;
}

let connect bus =
  let mgr =
    OBus_proxy.make
      ~peer:(OBus_peer.make ~connection:bus ~name:"org.freedesktop.login1")
      ~path:["org"; "freedesktop"; "login1"]
  in
  let* session =
    match Sys.getenv_opt "XDG_SESSION_ID" with
    | None | Some "" ->
      Log.info (fun f -> f "No $XDG_SESSION_ID; using PID instead.");
      Mgr.get_session_by_pid mgr ~pid:(Unix.getpid ())
    | Some session_id ->
      Log.info (fun f -> f "Using session ID %S from $XDG_SESSION_ID@." session_id);
      Mgr.get_session mgr ~session_id
  in
  Lwt.return { session }

let take_control ?(force=false) t = Session.take_control t.session ~force
let release_control t = Session.release_control t.session

let interface t =
  (* Annoyingly, libinput requires us to do the RPC call within the callback,
     meaning we have to start a new mainloop. A possible work-around would be
     to fail all the devices but record which ones it wanted and open them
     outside the callback, then trigger a rescan. *)
  let open_restricted path flags =
    let info = Unix.stat path in
    let rdev = PosixTypes.Dev.of_int info.st_rdev in
    let major = Input.Interface.major rdev in
    let minor = Input.Interface.minor rdev in
    try
      Lwt_main.run begin
        let* fd, _paused = Session.take_device t.session ~major ~minor in
        let fd = Unix.dup fd in          (* Original FD will be closed by OBus *)
        if List.mem Unix.O_NONBLOCK flags then Unix.set_nonblock fd;
        (* obus always sets CLOEXEC, though not in a thread-safe way *)
        Lwt.return_ok fd
      end
    with ex ->
      let bt = Printexc.get_raw_backtrace () in
      match OBus_error.name ex with
      | "System.Error.ENODEV" -> Error Unix.ENODEV
      | "System.Error.EPERM" -> Error Unix.EPERM
      | "System.Error.ENOENT" -> Error Unix.ENOENT
      | _ -> Printexc.raise_with_backtrace ex bt
  in
  let close_restricted fd =
    let info = Unix.fstat fd in
    Unix.close fd;
    let rdev = PosixTypes.Dev.of_int info.st_rdev in
    let major = Input.Interface.major rdev in
    let minor = Input.Interface.minor rdev in
    Lwt_main.run begin
      Session.release_device t.session ~major ~minor
    end
  in
  { Input.Interface.open_restricted; close_restricted }
