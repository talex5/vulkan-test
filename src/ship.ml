let gravity = 0.002             (* Decrease vertical velocity by this much each frame *)
let engine_power = 0.01         (* Increase speed by this amount (at full thrust) each frame *)
let air_resistance = 0.01       (* Drag on the craft *)

(* For a more exciting version, try increasing gravity and engine power, e.g. *)
(*
let gravity = 0.005
let engine_power = 0.02
*)

open Eio.Std

module Vkt = Vk.Types
module A = Vulkan.A
module Vec3 = Vulkan.Vec3

let shader_code = [%blob "./ship.spv"]

module Vertex = struct
  type mark
  type t = mark Ctypes.structure
  let ctype : t Ctypes.typ = Ctypes.structure "Vertex"

  let pos = Ctypes.field ctype "pos" Vec3.view
  let colour = Ctypes.field ctype "colour" Vec3.view
  let normal = Ctypes.field ctype "normal" Vec3.view

  let () = Ctypes.seal ctype

  let make ~pos:p ~colour:c ~normal:n =
    let t = Ctypes.make ctype in
    Ctypes.setf t pos p;
    Ctypes.setf t colour c;
    Ctypes.setf t normal n;
    t

  let attr_desc =
    let make location field format =
      let offset = Ctypes.offsetof field in
      Vkt.Vertex_input_attribute_description.make ~binding:0 ~location ~format ~offset
    in
    Vkt.Vertex_input_attribute_description.array [
      make 0 pos       R32g32b32_sfloat;
      make 1 colour    R32g32b32_sfloat;
      make 2 normal    R32g32b32_sfloat;
    ]
end

module Model = struct
  type t = {
    buffer : Vkt.Buffer.t;
    indices_offset : Vkt.Device_size.t;
    index_count : int;
  }

  let vertices : Vec3.t array =
    let v x y z = Vec3.(0.5 *. v x y z) in
    [|
      v ( 0.0) ( 0.0) ( 0.5);
      v (-1.0) ( 2.0) (-0.5);
      v ( 1.0) ( 2.0) (-0.5);
      v (-3.0) (-1.0) (-0.5);
      v ( 3.0) (-1.0) (-0.5);
      v ( 0.0) (-1.5) (-0.5);
    |]

  (* For each face, give the three vertices and the face's colour.
     Vertices must be given clockwise, for culling.
     Note that colours and normals get attached to their primary vertex in the end,
     as we don't pass faces separately. Faces starting with the same vertex therefore
     get the same colour and normal. *)
  let faces =
    let c = Vec3.v in
    let front = c 0.4 0.4 1.0 in
    let side = c 0.0 0.2 1.0 in
    let back = c 0.0 0.0 0.8 in
    let bottom = c 0.5 0.5 0.5 in
    [
      0, 1, 2, front;
      2, 4, 0, side;
      4, 5, 0, back;
      5, 3, 0, back;
      3, 1, 0, side;

      1, 4, 2, bottom;
      1, 5, 4, bottom;
      1, 3, 5, bottom;
    ]

  let find_face primary_v =
    List.find (fun (a, _, _, _) -> a = primary_v) faces

  let indices =
    faces
    |> List.concat_map (fun (a, b, c, _) -> [a; b; c])
    |> List.map Unsigned.UInt16.of_int
    |> A.of_list Ctypes.uint16_t

  let vertices =
    vertices
    |> Array.mapi (fun i a ->
        let (_, b, c, colour) = find_face i in
        let b = vertices.(b) in
        let c = vertices.(c) in
        let normal = Vec3.(norm (cross (c - a) (b - a))) in
        Vertex.make ~pos:a ~colour ~normal
      )
    |> Array.to_list
    |> A.of_list Vertex.ctype

  let allocate ~sw ~device =
    let size = A.sizeof vertices + A.sizeof indices in
    let buffer =
      Vulkan.Buffer.create ~sw device size
        ~usage:Vkt.Buffer_usage_flags.(vertex_buffer + index_buffer)
        ~properties:Vkt.Memory_property_flags.(host_visible + host_coherent)
    in
    Switch.run (fun sw ->
        let mapped = Vulkan.Buffer.map ~sw buffer in
        let vmapped, imapped = A.split mapped (A.sizeof vertices) in
        Memcpy.copy vertices ~dst:vmapped;
        Memcpy.copy indices ~dst:imapped;
      );
    {
      buffer = buffer.buffer;
      indices_offset = Vkt.Device_size.of_int (A.sizeof vertices);
      index_count = A.length indices;
    }

  let record t cmd =
    let { buffer; indices_offset; index_count } = t in
    let offsets = A.of_list Vkt.Device_size.ctype [Vkt.Device_size.zero] in
    let buffers = A.of_list Vkt.Buffer.ctype_opt [Some buffer] in
    Vulkan.Cmd.bind_vertex_buffers cmd buffers ~first_binding:0 ~offsets;
    Vulkan.Cmd.bind_index_buffer cmd buffer indices_offset Uint16;
    Vulkan.Cmd.draw_indexed cmd
      ~first_index:0 ~index_count
      ~first_instance:0 ~instance_count:1
      ~vertex_offset:0
end

let bindings = Vulkan.Binding.[
  v 0 Uniform_buffer Vkt.Shader_stage_flags.(vertex + fragment);
]

let vertices = Vkt.Pipeline_vertex_input_state_create_info.make ()
    ~vertex_binding_descriptions:(Vkt.Vertex_input_binding_description.(array [
        make ~binding:0 ~stride:(Ctypes.sizeof Vertex.ctype) ~input_rate:Vertex;
      ]))
    ~vertex_attribute_descriptions:Vertex.attr_desc

let no_stencil_op = Vkt.Stencil_op_state.make
    ~fail_op:Vkt.Stencil_op.Zero
    ~pass_op:Vkt.Stencil_op.Zero
    ~depth_fail_op:Vkt.Stencil_op.Zero
    ~compare_op:Vkt.Compare_op.Never
    ~compare_mask:0
    ~write_mask:0
    ~reference:0

let depth_testing = Vkt.Pipeline_depth_stencil_state_create_info.make ()
    ~depth_test_enable:true
    ~depth_write_enable:true
    ~depth_compare_op:Less
    ~depth_bounds_test_enable:false
    ~stencil_test_enable:false
    ~front:no_stencil_op
    ~back:no_stencil_op
    ~min_depth_bounds:0.0
    ~max_depth_bounds:0.0

type t = {
  state : Ubo.ship;
  particles : Particles.t;
  draw : (Vulkan.Cmd.t -> unit) Double.t;
  end_game_timer : (string * int) option ref;
}

let create ~sw ~device ~ubo ~render_pass ~particles =
  let state =
    let x, y = Map.random_start_location () in
    let z = Map.elevation x y in
    {
      Ubo.pos = Vec3.v (float x) (float y) (z +. 7.0);
      vel = Vec3.v 0.0 0.8 (gravity *. 100.0);
      pitch = 0.0;
      yaw = 0.0;
    } in
  let shader = Vulkan.Shader.load ~sw device shader_code in
  let max_sets = 2 in
  let set_layout = Vulkan.Descriptor_set.make_layout ~sw device bindings in
  let pipeline_layout =
    Vulkan.Device.create_pipeline_layout ~sw device @@
    Vkt.Pipeline_layout_create_info.make ()
      ~set_layouts:(Vkt.Descriptor_set_layout.array [set_layout])
  in
  let pool = Vulkan.Descriptor_set.create_pool ~sw device ~max_sets [
      Uniform_buffer, max_sets;
    ] in
  let pipeline =
    let rasterizer = Vkt.Pipeline_rasterization_state_create_info.make ()
        ~depth_clamp_enable:false
        ~rasterizer_discard_enable:false
        ~polygon_mode:Fill
        ~line_width:1.0
        ~cull_mode:Vkt.Cull_mode_flags.back
        ~front_face:Clockwise
        ~depth_bias_enable:false
        ~depth_bias_constant_factor:0.0
        ~depth_bias_clamp:0.0
        ~depth_bias_slope_factor:0.0
    in
    Vulkan.Pipeline.make ~sw ~device ~render_pass ()
      ~vertex_input_state:vertices
      ~topology:Triangle_list
      ~rasterizer
      ~layout:pipeline_layout
      ~depth_stencil_state:depth_testing
      ~stages:[
        shader "vertShip" Vkt.Shader_stage_flags.vertex;
        shader "fragShip" Vkt.Shader_stage_flags.fragment;
      ]
  in
  let model = Model.allocate ~sw ~device in
  let descriptor_sets = Vulkan.Descriptor_set.allocate pool set_layout max_sets in
  let end_game_timer = ref None in
  let draw =
    Double.init (fun side ->
        let descriptor_set = A.get descriptor_sets (Double.to_index side) in
        let ubo = Double.get ubo side in
        Vulkan.Descriptor_set.update ~device [
          Vulkan.Descriptor_set.write descriptor_set Uniform_buffer [ubo.Ubo.info] ~dst_binding:0 ~dst_array_element:0;
        ];
        fun cmd ->
          (* Note: set the ship position even if we're dead, as the other shaders need it too. *)
          Ubo.set_ship ubo
            ~ship_pos:state.pos
            ~ship_rot:Vulkan.Matrix4x4.(
                rot_z state.yaw *
                rot_x state.pitch
              );
          if !end_game_timer = None then (
            Vulkan.Cmd.bind_pipeline cmd ~stage:Graphics pipeline;
            Vulkan.Cmd.bind_descriptor_sets cmd [descriptor_set]
              ~pipeline_bind_point:Graphics
              ~layout:pipeline_layout
              ~first_set:0;
            Model.record model cmd
          )
      )
  in
  { state; particles; draw; end_game_timer }

let draw t side cmd = Double.get t.draw side cmd

let wrap { Vec3.x; y; z } : Vec3.t =
  let w v max =
    let max = float max in
    let v = Float.rem v max in
    if v < 0. then v +. max else v
  in
  {
    x = w x (fst Map.size);
    y = w y (snd Map.size);
    z;
  }

let update t (pointer : Surface.pointer_state) =
  match !(t.end_game_timer) with
  | Some (reason, 0) -> `Game_over reason
  | Some (reason, i) -> t.end_game_timer := Some (reason, i - 1); `Continue
  | None ->
    let state = t.state in
    let pointer_x = pointer.x -. 0.5 in
    let pointer_y = pointer.y -. 0.5 in
    let on_pad =
      Map.in_pad state.pos.x state.pos.y &&
      state.pos.z -. 1.0 < Map.pad_elevation
    in
    state.yaw <- -. Float.atan2 pointer_x (-. pointer_y);
    let pitch = -. 5.0 *. Float.sqrt (pointer_x ** 2. +. pointer_y ** 2.) in
    state.pitch <- if on_pad then max (-0.3) pitch else pitch;
    let thrust = pointer.thrust *. engine_power in
    let accel =
      Vec3.v
        (thrust *. sin state.yaw *. sin state.pitch)
        (-. thrust *. cos state.yaw *. sin state.pitch)
        (thrust *. cos state.pitch -. gravity)
    in
    if pointer.thrust > 0.0 then Particles.add_thrust t.particles state;
    let drag = 1.0 -. if on_pad then 0.1 else (air_resistance *. Vec3.mag state.vel ** 2.0) in
    let vel = Vec3.(drag *. state.vel + accel) in
    let vel = if on_pad then { vel with z = max vel.z (Map.pad_elevation -. state.pos.z) } else vel in
    state.vel <- vel;
    let pos = Vec3.(wrap (state.pos + vel)) in
    let surface_z = Map.elevation (truncate state.pos.x) (truncate state.pos.y) in
    if pos.z < surface_z +. 0.5 then (
      state.pos <- { pos with z = surface_z +. 0.5 };
      state.vel <- { state.vel with z = 0.0 };
      if not on_pad then (
        let reason =
          match Map.tile_type pos.x pos.y with
          | `Sea -> "killed by calm water"
          | `Hill -> "killed by a patch of ground"
          | `Lava -> "killed by lava"
        in
        t.end_game_timer := Some (reason, 150);
        Particles.add_explosion t.particles state.pos;
      )
    ) else (
      state.pos <- pos
    );
    `Continue

let pos t = t.state.pos
