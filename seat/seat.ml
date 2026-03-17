open Eio.Std
module Logind = Logind

let ( let* ) = Lwt.bind

type t =
  | Logind of Logind.t
  | Fallback of string

let connect ~sw () =
  try
    Lwt_main.run begin
      let* system_bus = OBus_bus.system () in
      let* logind = Logind.connect system_bus in
      let* () = Logind.take_control logind in
      Switch.on_release sw (fun () -> Lwt_main.run (Logind.release_control logind));
      Lwt.return (Logind logind)
    end
  with
  | Unix.Unix_error _ as ex -> Fallback (Printexc.to_string ex)
  | ex ->
    let bt = Printexc.get_raw_backtrace () in
    let (name, err) = OBus_error.cast ex in
    if name = OBus_error.ocaml then Printexc.raise_with_backtrace ex bt
    else Fallback (Printf.sprintf "%s: %s" name err)

let interface = function
  | Logind x -> Logind.interface x
  | Fallback _ -> Input.Interface.unix_direct
