open Common

type t = {
  device : Device.t;
  buffer : Vkt.Buffer.t;
  memory : Vkt.Device_memory.t;
  size : int;
}

let create ~sw ~usage ~properties device size =
  let dev = Device.dev device in
  let create_info = Vkt.Buffer_create_info.make ~size:(Vkt.Device_size.of_int size) ~usage ~sharing_mode:Exclusive () in
  let buffer = Vkc.create_buffer ~device:dev ~create_info () <?> "create_buffer" in
  Switch.on_release sw (fun () -> Vkc.destroy_buffer dev (Some buffer) None);
  let mem_requirements = Vkc.get_buffer_memory_requirements ~device:dev buffer in
  let memory_type_index =
    Device.find_memory_type device
      ~type_filter:(Vkt.Memory_requirements.memory_type_bits mem_requirements)
      ~properties
  in
  let allocate_info = Vkt.Memory_allocate_info.make ()
      ~allocation_size:(Vkt.Memory_requirements.size mem_requirements)
      ~memory_type_index
  in
  let memory = Vkc.allocate_memory ~device:dev ~allocate_info () <?> "allocate_memory" in
  Switch.on_release sw (fun () -> Vkc.free_memory dev (Some memory) None);
  Vkc.bind_buffer_memory ~device:dev buffer memory ~memory_offset:Vkt.Device_size.zero <?> "bind_buffer_memory";
  { device; buffer; memory; size }

let map ?(flags=Vkt.Memory_map_flags.empty) ?(offset=Vkt.Device_size.zero) ~sw t =
  let { device; memory; size; _ } = t in
  let p = Vkc.map_memory ~device:(Device.dev device) memory ~offset ~size:(Vkt.Device_size.of_int size) ~flags () <?> "map_memory" in
  Switch.on_release sw (fun () -> Vkc.unmap_memory (Device.dev device) memory);
  A.from_ptr Ctypes.(from_voidp char p) size
