open Common

module Dev_t = Drm.Dev_t

let required_extensions = [
  "VK_KHR_external_memory_capabilities";        (* Share images over Wayland *)
  "VK_KHR_external_semaphore_capabilities";     (* Use Linux sync files *)
]

let v major minor patch = (major lsl 22) lor (minor lsl 12) lor patch

let application_info ~version:(a, b, c) application_name =
  Vkt.Application_info.make ()
    ~application_version:(v a b c)
    ~application_name
    ~engine_version:(v 1 0 0)
    ~api_version:(v 1 2 0)

let create ?flags ?(validation_layers=[]) ?(extensions=[]) ~sw application_info =
  let create_info =
    Vkt.Instance_create_info.make ()
      ?flags
      ~application_info
      ~enabled_extension_names:(string_array (extensions @ required_extensions))
      ~enabled_layer_names:(string_array validation_layers)
  in
  let t = Vkc.create_instance ~create_info () <?> "create_instance" in
  Switch.on_release sw (fun () -> Vkc.destroy_instance (Some t) None);
  t

let render dev = Dev_t.make Vkt.Physical_device_drm_properties_ext.(render_major dev, render_minor dev)
let primary dev = Dev_t.make Vkt.Physical_device_drm_properties_ext.(primary_major dev, primary_minor dev)

let get_physical_device_properties x =
  let module P = Vkt.Physical_device_properties in
  let module P2 = Vkt.Physical_device_properties_2 in
  let module Drm = Vkt.Physical_device_drm_properties_ext in
  let drm_props =
    let t = Drm.unsafe_make () in
    Ctypes.setf t Drm.Fields.s_type Vk.Types.Structure_type.Physical_device_drm_properties_ext;
    t
  in
  let properties = P.unsafe_make () in        (* Shouldn't be needed *)
  let props2 =
    P2.make ~next:(ext (Drm.addr drm_props)) ~properties () in
  Vk.Raw.get_physical_device_properties_2 x (P2.addr props2);
  P.device_name (P2.properties props2), drm_props

let find_device instance wayland_device =
  Log.info (fun f -> f "Wayland compositor main device is %a" Dev_t.pp wayland_device);
  let physical_devices = Vkc.enumerate_physical_devices ~instance <?> "physical device" in
  Log.info (fun f -> f "Vulkan found %d physical devices" (A.length physical_devices));
  let found = ref None in
  physical_devices |> A.iteri (fun i x ->
      let device_name, drm_props = get_physical_device_properties x in
      Log.info (fun f -> f "%d: %s" i device_name);
      Log.debug (fun f -> f "dev: %a" Vkt.Physical_device_drm_properties_ext.pp drm_props);
      if render drm_props = wayland_device then found := Some (i, "rendering")
      else if primary drm_props = wayland_device then found := Some (i, "primary")
    );
  match !found with
  | None -> failwith "Wayland GPU device not found"
  | Some (i, x) ->
    Log.debug (fun f -> f "Using device %d (matches Wayland %s node)" i x);
    A.get physical_devices i
