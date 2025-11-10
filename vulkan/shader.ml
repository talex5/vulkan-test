open Common

module Bt = Vk__builtin__types

(* Loading a module would be easy, except that Vulkan wants a list of uint32s
   rather than a list of bytes. *)
let load ~sw device s =
  let len = String.length s in
  let c = A.make Bt.uint_32_t (len / Ctypes.(sizeof uint32_t)) in
  let c' = A.from_ptr Ctypes.(coerce (ptr Bt.uint_32_t) (ptr char) @@ A.start c) len in
  String.iteri (A.set c') s;
  fun name stage ->
    let create_info = Vkt.Shader_module_create_info.make ~code:c ~code_size:(Unsigned.Size_t.of_int len) () in
    let m = Vkc.create_shader_module ~device:(Device.dev device) ~create_info () <?> "create_shader_module" in
    Switch.on_release sw (fun () -> Vkc.destroy_shader_module (Device.dev device) (Some m) None);
    Vkt.Pipeline_shader_stage_create_info.make ~stage ~module':m ~name ()
