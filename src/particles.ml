module Vkt = Vk.Types
module A = Vulkan.A
module Vec3 = Vulkan.Vec3

let shader_code = [%blob "./particles.spv"]

let bindings = Vulkan.Binding.[
  v 0 Uniform_buffer Vkt.Shader_stage_flags.vertex;
]

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

module Thrust = struct
  type particle = {
    vel : Vec3.t;
    mutable pos : Vec3.t;
    mutable ttl : int;
  }

  type t = particle Queue.t

  let capacity = Ubo.max_particles / 2
  let max_ttl = 50

  let create () = Queue.create ()

  let add t (ship : Ubo.ship) =
    if Queue.length t = capacity then Queue.drop t;
    let speed = 0.5 in
    let pitch = ship.pitch +. (Random.float 0.5 -. 0.25) in
    let yaw = ship.yaw +. (Random.float 0.5 -. 0.25) in
    let vel =
      Vec3.v
        (-. speed *. sin yaw *. sin pitch)
        (speed *. cos yaw *. sin pitch)
        (-. speed *. cos pitch)
    in
    let pos = Vec3.(ship.pos + (0.5 +. Random.float 5.0) *. vel) in
    let vel = Vec3.(ship.vel + vel) in
    let p = { vel; pos; ttl = max_ttl } in
    Queue.push p t

  let write t ubo =
    let arr = Ubo.get_thrust ubo in
    let i = ref 0 in
    let write p =
      let slot = A.get arr !i in
      Ctypes.setf slot Ubo.C.Particle.pos p.pos;
      Ctypes.setf slot Ubo.C.Particle.brightness (float p.ttl /. float max_ttl);
      incr i
    in
    Queue.iter write t

  let count t = Queue.length t

  let update t =
    begin match Queue.peek_opt t with
      | Some { ttl = 0; _ } -> Queue.drop t
      | _ -> ()
    end;
    t |> Queue.iter (fun p ->
        p.pos <- Vec3.(p.pos + p.vel);
        p.ttl <- p.ttl - 1;
      )
end

type t = {
  thrust : Thrust.t;
  draw : (Vulkan.Cmd.t -> unit) Double.t;
}

let create ~sw ~device ~ubo ~render_pass =
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
        ~cull_mode:Vkt.Cull_mode_flags.none
        ~front_face:Clockwise
        ~depth_bias_enable:false
        ~depth_bias_constant_factor:0.0
        ~depth_bias_clamp:0.0
        ~depth_bias_slope_factor:0.0
    in
    Vulkan.Pipeline.make ~sw ~device ~render_pass ()
      ~topology:Point_list
      ~rasterizer
      ~layout:pipeline_layout
      ~depth_stencil_state:depth_testing
      ~stages:[
        shader "vertMain" Vkt.Shader_stage_flags.vertex;
        shader "fragMain" Vkt.Shader_stage_flags.fragment;
      ]
  in
  let descriptor_sets = Vulkan.Descriptor_set.allocate pool set_layout max_sets in
  let thrust = Thrust.create () in
  let draw =
    Double.init (fun side ->
        let descriptor_set = A.get descriptor_sets (Double.to_index side) in
        let ubo = Double.get ubo side in
        Vulkan.Descriptor_set.update ~device [
          Vulkan.Descriptor_set.write descriptor_set Uniform_buffer [ubo.Ubo.info] ~dst_binding:0 ~dst_array_element:0;
        ];
        fun cmd ->
          Thrust.write thrust ubo;
          Vulkan.Cmd.bind_pipeline cmd ~stage:Graphics pipeline;
          Vulkan.Cmd.bind_descriptor_sets cmd [descriptor_set]
            ~pipeline_bind_point:Graphics
            ~layout:pipeline_layout
            ~first_set:0;
          Vulkan.Cmd.draw cmd
            ~first_vertex:0 ~vertex_count:(Thrust.count thrust)
            ~first_instance:0 ~instance_count:1
      )
  in
  { thrust; draw }

let draw t side cmd =
  Double.get t.draw side cmd

let add_thrust t = Thrust.add t.thrust

let update t = Thrust.update t.thrust
