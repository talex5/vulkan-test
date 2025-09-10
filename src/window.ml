open Helpers

open Wayland_protocols.Xdg_shell_client
open Wayland.Wayland_client
module Registry = Wayland.Registry

module Dmabuf = struct
  open Wayland_protocols.Linux_dmabuf_unstable_v1_client

  type buffer = ([ `Wl_buffer ], [ `V1 ], [ `Client ]) Wayland.Proxy.t

  module Dmabuf_feedback_format_table = struct
    type t = Cstruct.t

    let sizeof_entry = 16

    let empty = Cstruct.empty

    let of_bigarray ba =
      Cstruct.of_bigarray (Bigarray.array1_of_genarray ba)

    let get t i =
      let off = i * sizeof_entry in
      Drm.Format.v (Cstruct.HE.get_uint32 t off)
        ~modifier:(Cstruct.HE.get_uint64 t (off + 8))
  end

  module Wayland_array16 = struct
    type t = string

    let length t = String.length t / 2

    let iter fn t =
      for i = 0 to length t - 1 do
        fn @@ String.get_uint16_ne t (i * 2)
      done
  end

  type t = {
    linux_dmabuf : [`V4] Zwp_linux_dmabuf_v1.t;
    drm_format : Drm.Format.t;
    main_device : Drm.Dev_t.t;
  }

  let bind reg =
    Wayland.Registry.bind reg
      object
        inherit [_] Zwp_linux_dmabuf_v1.v4
        method on_modifier _ ~format:_ ~modifier_hi:_ ~modifier_lo:_ = ()
        method on_format _ ~format:_ = ()
      end

  let get_surface_feedback linux_dmabuf surface =
    let t, set_t = Promise.create () in
    let main_device = ref None in
    let drm_format = ref None in
    let _ = Zwp_linux_dmabuf_v1.get_surface_feedback linux_dmabuf ~surface @@ object
        inherit [_] Zwp_linux_dmabuf_feedback_v1.v4

        val mutable table = Dmabuf_feedback_format_table.empty

        method on_format_table _ ~fd ~size =
          Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
          let size = Int32.to_int size in
          let ba = Unix.map_file fd Char C_layout false [|size|] in
          table <- Dmabuf_feedback_format_table.of_bigarray ba

        method on_main_device _ ~device = main_device := Some (Drm.Dev_t.of_raw device)

        method on_tranche_formats _ ~indices =
          indices |> Wayland_array16.iter (fun index ->
              let item = Dmabuf_feedback_format_table.get table index in
              Log.debug (fun f -> f "feedback_tranche_formats: %a" Drm.Format.pp item);
              (* My card doesn't support modifiers, so use the DRM_FORMAT_RESERVED fallback *)
              if item.code = Drm.Format.Code.xr24 && item.modifier = Drm.Format.Mod.reserved then (
                Log.info (fun f -> f "Found supported format %a" Drm.Format.pp item);
                drm_format := Some item
              )
            )

        method on_tranche_target_device _ ~device:_ = ()
        method on_tranche_flags _ ~flags:_ = ()
        method on_tranche_done _ = ()

        method on_done proxy =
          Zwp_linux_dmabuf_feedback_v1.destroy proxy;
          if not (Promise.is_resolved t) then (
            Promise.resolve set_t @@
            match !drm_format, !main_device with
            | None, _ -> Error (Failure "No supported DRM format found")
            | _, None -> Error (Failure "Wayland compositor didn't send a main device!")
            | Some drm_format, Some main_device -> Ok { linux_dmabuf; drm_format; main_device }
          )
      end
    in
    t

  (* Register a dmabuf with the Wayland compositor *)
  let create_buffer ~sw ~on_release ~offset ~stride ~width ~height ~fd t =
    let modifier = t.drm_format.modifier in
    let params = Zwp_linux_dmabuf_v1.create_params t.linux_dmabuf @@ object
        inherit [_] Zwp_linux_buffer_params_v1.v1
        method on_created _ = assert false        (* Not used for immediate creation *)
        method on_failed _ =
          (* The spec says sending this is optional, so it's a bit pointless. *)
          Log.warn (fun f -> f "create_buffer failed")
      end
    in
    Eio_unix.Fd.use_exn "create_buffer" fd @@ fun fd ->
    Zwp_linux_buffer_params_v1.add params
      ~fd
      ~plane_idx:0l
      ~offset
      ~stride
      ~modifier_hi:(Int64.shift_right_logical modifier 32 |> Int64.to_int32)
      ~modifier_lo:(Int64.to_int32 modifier);
    let buffer = Zwp_linux_buffer_params_v1.create_immed params ~width ~height ~format:t.drm_format.code ~flags:0l @@ object
        inherit [_] Wl_buffer.v1
        method on_release _ = on_release ()
      end
    in
    Zwp_linux_buffer_params_v1.destroy params;
    Switch.on_release sw (fun () -> Wl_buffer.destroy buffer);
    buffer
end

type t = {
  display : Wayland.Client.t;
  wayland_dmabuf : Dmabuf.t;
  surface : [`V4] Wl_surface.t;
}

exception Closed

let init ~sw transport =
  (* Connect to the Wayland compositor *)
  let display = Wayland.Client.connect ~sw transport in

  (* Get the list of extensions and bind the ones we want *)
  let reg = Registry.of_display display in
  let compositor = Registry.bind reg (new Wl_compositor.v4) in
  let xdg_wm_base = Registry.bind reg @@ object
      inherit [_] Xdg_wm_base.v1
      method on_ping = Xdg_wm_base.pong
    end
  in
  let linux_dmabuf = Dmabuf.bind reg in

  (* Create the window *)
  let surface = Wl_compositor.create_surface compositor @@ object
      inherit [_] Wl_surface.v1
      method on_enter _ ~output:_ = ()
      method on_leave _ ~output:_ = ()
      method on_preferred_buffer_scale _ ~factor:_ = ()
      method on_preferred_buffer_transform _ ~transform:_ = ()
    end
  in
  let configured, set_configured = Promise.create () in
  let xdg_surface = Xdg_wm_base.get_xdg_surface xdg_wm_base ~surface @@ object
      inherit [_] Xdg_surface.v1
      method on_configure proxy ~serial =
        Xdg_surface.ack_configure proxy ~serial;
        if not (Promise.is_resolved configured) then
          Promise.resolve set_configured ()
    end
  in
  let toplevel = Xdg_surface.get_toplevel xdg_surface @@ object
      inherit [_] Xdg_toplevel.v1
      method on_configure_bounds _ ~width:_ ~height:_ = ()
      method on_configure _ ~width:_ ~height:_ ~states:_ = ()
      method on_close _ = Switch.fail sw Closed
      method on_wm_capabilities _ ~capabilities:_ = ()
    end
  in
  (* Ask for information about which GPU to use for this surface *)
  let wayland_dmabuf = Dmabuf.get_surface_feedback linux_dmabuf surface in
  Xdg_toplevel.set_title toplevel ~title:"ocaml-vulkan-test";
  Wl_surface.commit surface;
  Promise.await configured;
  let wayland_dmabuf = Promise.await_exn wayland_dmabuf in
  { display; surface; wayland_dmabuf }
