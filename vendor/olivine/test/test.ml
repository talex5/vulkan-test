module Vkc = Vk.Core
module Vkt = Vk.Types
module L = Info.Linguistic

let ( !@ ) = Ctypes.( !@ )

let test_struct_string () =
  let name = "test string" in
  let module M = Vkt.Pipeline_shader_stage_create_info in
  let x = M.make ()
      ~stage:Vkt.Shader_stage_flags.empty
      ~module':Vkt.Shader_module.null
      ~name
  in
  Alcotest.(check string) "Name before GC" name @@ M.name x;
  Gc.full_major ();
  Alcotest.(check string) "Name after GC" name @@ M.name x

let test_struct_ptr () =
  let application_name = "test string" in
  let module A = Vkt.Application_info in
  let module I = Vkt.Instance_create_info in
  let application_info =
    A.make ()
      ~application_name
      ~application_version:1
      ~api_version:1
      ~engine_version:1
  in
  let x = I.make ~application_info () in
  let info x = I.application_info x |> Option.get in
  let name x = !@ (info x) |> A.application_name |> Option.get in
  Alcotest.(check string) "Name before GC" application_name @@ name x;
  Gc.full_major ();
  Alcotest.(check string) "Name after GC" application_name @@ name x

let test_union_string () =
  let s = "test string" in
  let module M = Vkt.Performance_value_data_intel in
  let x = M.value_string s in
  let get x = Ctypes.(coerce (ptr char) string) @@ Ctypes.getf x M.Fields.value_string in
  Alcotest.(check string) "String before GC" s @@ get x;
  Gc.full_major ();
  Alcotest.(check string) "String after GC" s @@ get x

let test_union_ptr () =
  let s = "test string" in
  let module M = Vkt.Device_or_host_address_khr in
  let x = M.host_address (Ctypes.(coerce string (ptr void)) s) in
  let get x = Ctypes.(coerce (ptr void) string) @@ Ctypes.getf x M.Fields.host_address in
  Alcotest.(check string) "String before GC" s @@ get x;
  Gc.full_major ();
  Alcotest.(check string) "String after GC" s @@ get x

(* Mostly we just care that this test compiles.
   Note: some of these names are bad; the test should be updated if they improve
   but helps to track what's changed. *)
let bit n = 1 lsl n
let test_awkward_names () =
  (* Annoying extra underscore makes this look ugly: *)
  Alcotest.(check int) "Format" 97 Vkt.Format.(to_int R16g16b16a16_sfloat);
  (* A bit-field with no values: *)
  Alcotest.(check int) "VkDeviceCreateFlags" 0 Vkt.Device_create_flags.(to_int empty);
  (* Android is an extension, but also used within the name: *)
  Alcotest.(check int) "Android-Android" (bit 10)
    Vkt.External_memory_handle_type_flags.(to_int android_hardware_buffer_bit_android);
  (* The leading n2 shouldn't be here: *)
  Alcotest.(check int) "Pipeline flags" (bit 15) Vkt.Pipeline_stage_flags_2_khr.(to_int all_graphics);
  ()
module type S2 = module type of Vk.Amd.Shader_core_properties_2 (* Extension sub-module with 2 in its name *)

let name = Alcotest.of_pp L.full_pp

let dict, _exts =
  let module M = Info.Common.StringMap in
  let spec : Info.Structured_spec.t = {
    aliases = M.empty;
    vendor_ids = [];
    tags = [ { name = "KHR"; author = ""; contact = "" } ];
    entities = M.empty;
    updates = [];
    includes = [];
    requires = [];
    extensions = [];
  } in
  Info.Vulkan_dialect.make spec

let mk_name ?(prefix=[]) ?(postfix=[]) main =
  { L.prefix; main; postfix = List.rev postfix }

let test_flag_bits2 () =
  let ctx = L.make dict "VkPipelineStageFlagBits2KHR" in
  Alcotest.check name "Context" (mk_name ["pipeline";"stage";"flag";"bits";"2"] ~postfix:["khr"]) ctx;
  let ctx = Aster.Bitset.set_name_stem ctx in
  Alcotest.check name "Stem" (mk_name ["pipeline";"stage";"2"] ~postfix:["khr"]) ctx;
  let constr = L.make dict "VK_PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR" in
  Fmt.epr "ctx    = %a@." L.full_pp ctx;
  Fmt.epr "constr = %a@." L.full_pp constr;
  let expected = mk_name ["vertex"; "input"] in
  Alcotest.check name "Remove prefix" expected (Aster.Bitset.field_name ctx constr)

let test_link () =
  match Vkc.enumerate_instance_version () with
  | Ok (`Success, v) -> Fmt.epr "Linked Vulkan version %d" v
  | Error e -> Fmt.failwith "%a" Vkt.Result.raw_pp e

let () =
  let open Alcotest in
  run "Olivine" [
    "gc", [
      test_case "struct-string"  `Quick test_struct_string;
      test_case "struct-ptr"     `Quick test_struct_ptr;
      test_case "union-string"   `Quick test_union_string;
      test_case "union-ptr"      `Quick test_union_ptr;
    ];
    "gen", [
      test_case "awkward-names"  `Quick test_awkward_names;
    ];
    "linguistic", [
      test_case "flag_bits2"     `Quick test_flag_bits2;
    ];
    "linking", [
      test_case "test_link"      `Quick test_link;
    ];
  ]
