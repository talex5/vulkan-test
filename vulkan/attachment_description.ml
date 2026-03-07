open Common

let make
    ?(samples=Vkt.Sample_count_flags.n1)
    ?(initial_layout=Vkt.Image_layout.Undefined)
    ?(final_layout=Vkt.Image_layout.General)
    ?(stencil_load_op=Vkt.Attachment_load_op.Dont_care)
    ?(stencil_store_op=Vkt.Attachment_store_op.Dont_care)
    ~load_op
    ~store_op
    format =
  Vkt.Attachment_description.make ()
    ~format
    ~samples
    ~load_op
    ~store_op
    ~stencil_load_op
    ~stencil_store_op
    ~initial_layout
    ~final_layout
