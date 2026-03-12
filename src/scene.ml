module Vkt = Vk.Types
module A = Vulkan.A
module Vec3 = Vulkan.Vec3

let projection =
  if true then `Perspective
  else `Overhead (* For debugging *)

let float_array = A.of_list Ctypes.float        (* Doesn't require GC protection *)

type t = {
  render_pass : Vkt.Render_pass.t;
  ubo : Ubo.t Double.t;
  landscape : Landscape.t;
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
  let landscape = Landscape.create ~sw ~device ~ubo ~render_pass in
  let ship = Ship.create ~sw ~device ~ubo ~render_pass in
  { render_pass; ubo; landscape; ship; frame = 0 }

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
let camera_to_ship = 43.0
let ship_to_fog = 30.0
let fov_y_ideal = tau /. 12.0

let fov_x_min = fov_y_ideal

(* Calculate the maximum horizontal field of view so that we can't see the edge of the landscape. *)
let fov_x_max =
  let vw, _ = Ubo.land_view_size in
  2. *. Float.atan2 (float (vw - 4) /. 2.) (camera_to_ship +. ship_to_fog)

(* Configure the uniform buffer with the camera details. *)
let configure_camera t ubo (width, height) =
  let ship_pos = Ship.pos t.ship in
  (* The landscape display is a bit odd: it follows the ship around
     and just shows different bits of the map. *)
  let land_visible_nw =
    let view_width, _view_height = Ubo.land_view_size in
    (ship_pos.x -. float view_width /. 2.,
     ship_pos.y +. ship_to_fog)
  in
  match projection with
  | `Overhead ->
    let camera_pos = {ship_pos with z = ship_pos.z +. 10.0} in
    let project_world = Vulkan.Matrix4x4.(
        rot_x (tau /. 2.) *             (* Convert to Vulkan clip space axes (Y down, Z into screen). *)
        scale 0.02 *
        Vulkan.Matrix4x4.look           (* This is just a translation to [camera_pos]. *)
          ~from:camera_pos
          ~at:{ship_pos with z = 0.0}
          ~up:(Vec3.v 0. 1. 0.)
      ) in
    Ubo.set_camera ubo ~camera_pos ~project_world ~land_visible_nw
  | `Perspective ->
    let aspect = width /. height in
    let fov_y, excess_window_height =
      let fov_x = fov_y_ideal *. aspect in
      if fov_x > fov_x_max then fov_x_max /. aspect, 0.0
      else if fov_x < fov_x_min then (
        let ideal_window_height = width /. (fov_x_min /. fov_y_ideal) in
        let excess_window_height = max 0.0 (height -. ideal_window_height) in
        fov_x_min /. aspect, excess_window_height
      ) else fov_y_ideal, 0.0
    in
    let camera_target = Vec3.v ship_pos.x ship_pos.y (max 6.0 ship_pos.z) in
    let camera_pos = { camera_target with
                       y = camera_target.y -. camera_to_ship;
                       z = camera_target.z +. 10.;
                     } in
    let up = Vec3.v 0.0 0.0 1.0 in
    let project_world = Vulkan.Matrix4x4.(
        translate (Vec3.v 0.0 (excess_window_height /. height) 0.0) *
        perspective_projection ~fov_y ~aspect ~z_near ~z_far *
        rot_x (tau /. 2.) *
        Vulkan.Matrix4x4.look ~from:camera_pos ~at:camera_target ~up
      ) in
    Ubo.set_camera ubo ~camera_pos ~project_world ~land_visible_nw

let draw t side cmd framebuffer =
  let { Surface.framebuffer; geometry; _ } = framebuffer in
  let (width, height) = geometry in
  let ubo = Double.get t.ubo side in
  configure_camera t ubo (float width, float height);
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
      Landscape.draw t.landscape side cmd;
      Ship.draw t.ship side cmd;
    )
