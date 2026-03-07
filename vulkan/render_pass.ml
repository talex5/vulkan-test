open Common

let info ~attachments ~subpasses ?dependencies () =
  Vkt.Render_pass_create_info.make ()
    ~attachments:(Vkt.Attachment_description.array attachments)
    ~subpasses:(Vkt.Subpass_description.array subpasses)
    ?dependencies:(Option.map Vkt.Subpass_dependency.array dependencies)
