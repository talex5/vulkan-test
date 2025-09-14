open Eio.Std

open Wayland_protocols.Xdg_shell_client
open Wayland.Wayland_client
module Registry = Wayland.Registry

type t = {
  display : Wayland.Client.t;
  wayland_dmabuf : Vulkan.Dmabuf.t;
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
  let linux_dmabuf = Vulkan.Dmabuf.bind reg in

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
  let wayland_dmabuf = Vulkan.Dmabuf.get_surface_feedback linux_dmabuf surface in
  Xdg_toplevel.set_title toplevel ~title:"ocaml-vulkan-test";
  Wl_surface.commit surface;
  Promise.await configured;
  let wayland_dmabuf = Promise.await_exn wayland_dmabuf in
  { display; surface; wayland_dmabuf }
