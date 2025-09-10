include Eio.Std

module Bt = Vk__builtin__types
module Vkc = Vk.Core
module Vkt = Vk.Types
module A = Ctypes.CArray

let log fmt = Fmt.epr (fmt ^^ "@.")

let ( <?> ) x s = match x with
  | Ok (r, x) -> Log.debug (fun f -> f "%a: %s" Vkt.Result.raw_pp r s); x
  | Error k -> Fmt.failwith "Error %a: %s" Vkt.Result.raw_pp k s

(* For the [pNext] extension fields, we need to convert to void pointers explicitly. *)
let ext = Ctypes.to_voidp

(* [A.of_list Ctypes.string] doesn't work because it doesn't register the
   C strings with the GC. This is a work-around for that. *)
let string_array xs =
  let xs = List.map Ctypes.(coerce string (ptr char)) xs in     (* Make the C strings *)
  let arr = A.of_list Ctypes.(ptr char) xs in                   (* Make the array *)
  let p = Ctypes.to_voidp arr.astart |> Ctypes.(from_voidp string) in
  let arr = A.from_ptr p arr.alength in                         (* Cast to string array *)
  Vk__helpers.keep_alive xs arr;
  arr

(* Float arrays don't have the above problem, but add a helper so we don't
   have to remember when [A.of_list] is safe and when it isn't. *)
let float_array = A.of_list Ctypes.float

(* Loading a module would be easy, except that Vulkan wants a list of uint32s
   rather than a list of bytes. *)
let load_shader_module device s =
  let len = String.length s in
  let c = A.make Bt.uint_32_t (len / Ctypes.(sizeof uint32_t)) in
  let c' = A.from_ptr Ctypes.(coerce (ptr Bt.uint_32_t) (ptr char) @@ A.start c) len in
  String.iteri (A.set c') s;
  let create_info = Vkt.Shader_module_create_info.make ~code:c ~code_size:(Unsigned.Size_t.of_int len) () in
  Vkc.create_shader_module ~device ~create_info () <?> "create_shader_module"

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

let find_memory_type ~memory_properties ~type_filter ~properties =
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

let create_buffer ~device ~memory_properties ~usage ~properties size =
  let create_info = Vkt.Buffer_create_info.make ~size ~usage ~sharing_mode:Exclusive () in
  let buffer = Vkc.create_buffer ~device ~create_info () <?> "create_buffer" in
  let mem_requirements = Vkc.get_buffer_memory_requirements ~device buffer in
  let memory_type_index =
    find_memory_type
      ~memory_properties
      ~type_filter:(Vkt.Memory_requirements.memory_type_bits mem_requirements)
      ~properties
  in
  let allocate_info = Vkt.Memory_allocate_info.make ()
      ~allocation_size:(Vkt.Memory_requirements.size mem_requirements)
      ~memory_type_index
  in
  let memory = Vkc.allocate_memory ~device ~allocate_info () <?> "allocate_memory" in
  Vkc.bind_buffer_memory ~device buffer memory ~memory_offset:Vkt.Device_size.zero <?> "bind_buffer_memory";
  buffer, memory

let show_extensions () =
  let extensions = Vkc.enumerate_instance_extension_properties () <?> "enumerate_instance_extension_properties" in
  log "Extentions: %d" (A.length extensions);
  A.iter (fun e -> log "- %s" (Vkt.Extension_properties.extension_name e)) extensions

let create_export_semaphore device =
  let export_info = Vkt.Export_semaphore_create_info.make ~handle_types:Vkt.External_semaphore_handle_type_flags.sync_fd () in
  let create_info = Vkt.Semaphore_create_info.make () ~next:(ext (Vkt.Export_semaphore_create_info.addr export_info)) in
  Vkc.create_semaphore ~device ~create_info () <?> "create_semaphore"
