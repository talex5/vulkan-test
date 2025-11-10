open Common

let make_layout ~sw device bindings =
  let create_info = Vkt.Descriptor_set_layout_create_info.make ()
      ~bindings:(Vkt.Descriptor_set_layout_binding.array bindings)
  in
  let x = Vkc.create_descriptor_set_layout ~device:(Device.dev device) ~create_info () <?> "createDescriptorSetLayout" in
  Switch.on_release sw (fun () -> Vkc.destroy_descriptor_set_layout (Device.dev device) (Some x) None);
  x

let make_size (typ, descriptor_count) = Vkt.Descriptor_pool_size.make ~typ ~descriptor_count

let create_pool ~sw ~max_sets device sizes =
  let device = Device.dev device in
  let pool_sizes = List.map make_size sizes in
  let create_info = Vkt.Descriptor_pool_create_info.make ()
      ~pool_sizes:(Vkt.Descriptor_pool_size.array pool_sizes)
      ~max_sets
  in
  let pool = Vkc.create_descriptor_pool ~device ~create_info () <?> "create_descriptor_pool" in
  Switch.on_release sw (fun () -> Vkc.destroy_descriptor_pool device (Some pool) None);
  device, pool

let allocate (device, descriptor_pool) layout n =
  let set_layouts = Vkt.Descriptor_set_layout.array (List.init n (Fun.const layout)) in
  let allocate_info = Vkt.Descriptor_set_allocate_info.make ~descriptor_pool ~set_layouts () in
  Vkc.allocate_descriptor_sets ~device ~allocate_info <?> "allocate_descriptor_sets"

type _ typ =
  | Uniform_buffer : Vkt.Descriptor_buffer_info.t typ
  | Combined_image_sampler : Vkt.Descriptor_image_info.t typ

let write (type a) dst_set ~dst_binding ~dst_array_element (typ : a typ) (values : a list) =
  let (buffer_info : Vkt.Descriptor_buffer_info.t list), (image_info : Vkt.Descriptor_image_info.t list) =
    match typ with
    | Uniform_buffer -> values, []
    | Combined_image_sampler -> [], values
  in
  let texel_buffer_view = Vkt.Buffer_view.array [] in
  Vkt.Write_descriptor_set.make ()
    ~dst_set
    ~dst_binding
    ~dst_array_element
    ~descriptor_type:(match typ with
        | Uniform_buffer -> Uniform_buffer
        | Combined_image_sampler -> Combined_image_sampler
      )
    ~descriptor_count:(List.length values)
    ~buffer_info:(Vkt.Descriptor_buffer_info.array buffer_info)
    ~image_info:(Vkt.Descriptor_image_info.array image_info)
    ~texel_buffer_view

let update device ~writes =
  Vkc.update_descriptor_sets ()
    ~device:(Device.dev device)
    ~descriptor_writes:(Vkt.Write_descriptor_set.array writes)
