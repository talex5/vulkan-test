open Common

module A = Common.A
module Dmabuf = Dmabuf
module Instance = Instance
module Device = Device
module Buffer = Buffer
module Shader = Shader
module Semaphore = Semaphore
module Fence = Fence
module Cmd = Cmd
module Image = Image
module Binding = Binding
module Descriptor_set = Descriptor_set
module Swap_chain = Swap_chain
module Drm_format = Drm_format

module Format = struct
  let of_drm (drm_format : Drm_format.t) =
    if drm_format.code = Drm.Fourcc.xr24 then Vkt.Format.B8g8r8a8_srgb
    else Fmt.failwith "Unknown DRM format %a" Drm_format.pp drm_format
end
