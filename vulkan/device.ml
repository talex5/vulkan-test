open Common

module type EXT = sig
  val import_semaphore_fd_khr : Vkt.Import_semaphore_fd_info_khr.t -> unit
  val get_semaphore_fd_khr : Vkt.Semaphore_get_fd_info_khr.t -> int
  val get_memory_fd_khr : Vkt.Memory_get_fd_info_khr.t -> int
end

module Ext(Device : sig val x : Vkt.Device.t end) : EXT = struct
  module External_memory_fd = Vk.Khr.External_memory_fd(Device)
  module External_semaphore = Vk.Khr.External_semaphore_fd(Device)

  let import_semaphore_fd_khr import_semaphore_fd_info =
    External_semaphore.import_semaphore_fd_khr ~import_semaphore_fd_info <?> "vkImportSemaphoreFdKHR"

  let get_semaphore_fd_khr get_fd_info =
    External_semaphore.get_semaphore_fd_khr ~get_fd_info <?> "get_semaphore_fd_khr"

  let get_memory_fd_khr get_fd_info =
    External_memory_fd.get_memory_fd_khr ~get_fd_info <?> "get_memory_fd_khr"
end

type t = {
  device : Vkt.Device.t;
  graphics_family : int;
  graphics_queue : Vkt.Queue.t;
  memory_properties : Vkt.Physical_device_memory_properties.t;
  ext : (module EXT);
}

let dev x = x.device

let is_graphics x =
  match Vkt.Queue_family_properties.queue_flags x with
  | Some flags -> Vkt.Queue_flags.(mem graphics) flags
  | None -> false

let find_graphics_family physical_device =
  let queue_families = Vkc.get_physical_device_queue_family_properties physical_device in
  Log.debug (fun f -> f "Device has %d queue families" (A.length queue_families));
  match List.find_index is_graphics @@ A.to_list queue_families with
  | None -> failwith "No graphics queue!"
  | Some i ->
    Log.debug (fun f -> f "Found graphics queue family (%d)" i);
    i

let create ~sw physical_device =
  (* Create logical device with one graphics queue *)
  let graphics_family = find_graphics_family physical_device in
  let queue_create_infos =
    Vkt.Device_queue_create_info.array [
      Vkt.Device_queue_create_info.make ()
        ~queue_family_index:graphics_family
        ~queue_priorities:(float_array [1.0])
    ] in
  let enabled_extension_names = string_array [
      (* Export image FD so we can send it to the compositor *)
      "VK_KHR_external_memory";
      "VK_KHR_external_memory_fd";
      "VK_EXT_external_memory_dma_buf";
      (* Export semaphores to synchronise with the compositor *)
      "VK_KHR_external_semaphore";
      "VK_KHR_external_semaphore_fd";
    ] in
  let create_info = Vkt.Device_create_info.make ~queue_create_infos ~enabled_extension_names () in
  let device = Vkc.create_device () ~physical_device ~create_info <?> "create_device" in
  Switch.on_release sw (fun () -> Vkc.destroy_device (Some device) None);
  let graphics_queue = Vkc.get_device_queue device ~queue_family_index:graphics_family ~queue_index:0 in
  let ext = (module Ext(struct let x = device end) : EXT) in
  let memory_properties = Vkc.get_physical_device_memory_properties physical_device in
  { device; graphics_family; graphics_queue; ext; memory_properties }

let find_memory_type t ~type_filter ~properties =
  let memory_properties = t.memory_properties in
  let avail = Vkt.Physical_device_memory_properties.memory_types memory_properties in
  let ok x =
    let flags = Vkt.Memory_type.property_flags x |> Option.value ~default:Vkt.Memory_property_flags.empty in
    Vkt.Memory_property_flags.(properties * flags = properties)
  in
  let rec aux i = function
    | [] -> failwith "failed to find suitable memory type!"
    | x :: xs ->
      if (type_filter land (1 lsl i) <> 0) && ok x then i
      else aux (i + 1) xs
  in aux 0 (A.to_list avail)

let wait_idle t =
  Vkc.device_wait_idle t.device <?> "device_wait_idle"

let create_render_pass ~sw t create_info =
  let x = Vkc.create_render_pass ~device:t.device ~create_info () <?> "create_render_pass" in
  Switch.on_release sw (fun () -> Vkc.destroy_render_pass t.device (Some x) None);
  x

let create_pipeline_layout ~sw t create_info =
  let x = Vkc.create_pipeline_layout ~device:t.device ~create_info () <?> "create_pipeline_layout" in
  Switch.on_release sw (fun () -> Vkc.destroy_pipeline_layout t.device (Some x) None);
  x

let create_pipeline ~sw t info =
  let create_infos = Vkt.Graphics_pipeline_create_info.array [info] in
  let pipelines = Vkc.create_graphics_pipelines () ~device:t.device ~create_infos <?> "create_graphics_pipelines" in
  let x = A.get pipelines 0 in
  Switch.on_release sw (fun () -> Vkc.destroy_pipeline t.device (Some x) None);
  x

let create_sampler ~sw t create_info =
  let x = Vkc.create_sampler ~device:t.device ~create_info () <?> "create_sampler" in
  Switch.on_release sw (fun () -> Vkc.destroy_sampler t.device (Some x) None);
  x
