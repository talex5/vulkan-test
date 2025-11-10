open Common

let v ?(descriptor_count=1) binding descriptor_type stage_flags =
  Vkt.Descriptor_set_layout_binding.make ()
    ~binding
    ~descriptor_type
    ~descriptor_count
    ~stage_flags
