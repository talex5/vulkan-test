open Common

let make ?color_attachments ?depth_stencil_attachment pipeline_bind_point =
  let aref (attachment, layout) = Vkt.Attachment_reference.make ~attachment ~layout in
  let color_attachments = color_attachments |> Option.map (List.map aref) in
  let depth_stencil_attachment = depth_stencil_attachment |> Option.map aref in
  Vkt.Subpass_description.make ()
    ~pipeline_bind_point
    ?color_attachments:(Option.map Vkt.Attachment_reference.array color_attachments)
    ?depth_stencil_attachment
