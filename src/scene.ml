module Vkt = Vk.Types
module A = Vulkan.A
module Vec3 = Vulkan.Vec3

let float_array = A.of_list Ctypes.float        (* Doesn't require GC protection *)

type t = {
  render_pass : Vkt.Render_pass.t;
  ubo : Ubo.t Double.t;
  ship : Ship.t;
  mutable frame : int;
}

let create ~sw ~format ~device =
  let attachments = [
    (* The render needs a colour image, which it clears and then writes to: *)
    Vulkan.Attachment_description.make format
      ~load_op:Clear	(* Clear framebuffer before rendering *)
      ~store_op:Store
      ~initial_layout:Undefined
      ~final_layout:General;
    (* and a depth buffer so things near things are drawn in front of far things: *)
    Vulkan.Attachment_description.make D32_sfloat
      ~load_op:Clear
      ~store_op:Dont_care
      ~initial_layout:Undefined
      ~final_layout:Depth_stencil_attachment_optimal;
  ] in
  (* There's only a single subpass, using both: *)
  let subpass = Vulkan.Subpass.make Graphics
      ~color_attachments:[0, Color_attachment_optimal]
      ~depth_stencil_attachment:(1, Depth_attachment_stencil_read_only_optimal)
  in
  (* Subpass 0 depends on external inputs (not other subpasses): *)
  let dependency = Vkt.Subpass_dependency.make ()
      ~src_subpass:(Unsigned.UInt.to_int Vk.Const.subpass_external)
      ~dst_subpass:0
      ~src_stage_mask:Vkt.Pipeline_stage_flags.(color_attachment_output + late_fragment_tests)
      ~dst_stage_mask:Vkt.Pipeline_stage_flags.(color_attachment_output + early_fragment_tests)
      ~src_access_mask:Vkt.Access_flags.depth_stencil_attachment_write
      ~dst_access_mask:Vkt.Access_flags.(depth_stencil_attachment_write + color_attachment_write)
  in
  let render_pass = Vulkan.Device.create_render_pass ~sw device @@
    Vulkan.Render_pass.info ()
      ~attachments
      ~subpasses:[subpass]
      ~dependencies:[dependency]
  in
  let ubo = Double.init (fun (_ : Double.side) -> Ubo.create ~sw ~device) in
  let ship = Ship.create ~sw ~device ~ubo ~render_pass in
  { render_pass; ubo; ship; frame = 0 }

let viewport ~width ~height =
  Vkt.Viewport.make
    ~x:0.0
    ~y:0.0
    ~width:(float width)
    ~height:(float height)
    ~min_depth:0.0
    ~max_depth:1.0

let rect ~x ~y ~width ~height =
  let offset = Vkt.Offset_2d.make ~x ~y in
  let extent = Vkt.Extent_2d.make ~width ~height in
  Vkt.Rect_2d.make ~offset ~extent

let tau = Float.pi *. 2.
let z_near = 1.
let z_far = 100.
let fov_y = (1. /. 12.) *. tau

let draw t side cmd framebuffer =
  let { Surface.framebuffer; geometry; _ } = framebuffer in
  let (width, height) = geometry in
  let ubo = Double.get t.ubo side in
  (* Configure the uniform buffer with the camera details. *)
  let aspect = float width /. float height in
  let camera_pos = Vec3.v 0.0 10.0 0.0 in
  let project_world = Vulkan.Matrix4x4.(
      perspective_projection ~fov_y ~aspect ~z_near ~z_far *
      rot_x (tau *. 90.0 /. 360.) *
      translate (Vec3.neg camera_pos)) in
  Ubo.set_camera ubo
    ~camera_pos
    ~project_world;
  (* Write the rendering operations to the command buffer. *)
  let black = Vkt.Clear_color_value.float_32 (float_array [0.0; 0.0; 0.0; 1.0]) in
  let far = Vkt.Clear_depth_stencil_value.make ~depth:1.0 ~stencil:0 in
  let clear_values = Vkt.Clear_value.array [
      Vkt.Clear_value.color black;
      Vkt.Clear_value.depth_stencil far;
    ] in
  let render_area = rect ~x:0 ~y:0 ~width ~height in
  let info = Vkt.Render_pass_begin_info.make ~render_pass:t.render_pass ~framebuffer ~render_area ~clear_values () in
  Vulkan.Cmd.render_pass cmd info ~subpass_contents:Inline (fun () ->
      Vulkan.Cmd.set_viewport cmd ~first_viewport:0 [viewport ~width ~height];
      Vulkan.Cmd.set_scissor cmd ~first_scissor:0 [render_area];
      Ship.draw t.ship side cmd;
    )
