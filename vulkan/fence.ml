open Common

let create ~sw device flags =
  let create_info = Vkt.Fence_create_info.make ~flags () in
  let t = Vkc.create_fence ~device:(Device.dev device) ~create_info () <?> "create_fence" in
  Switch.on_release sw (fun () -> Vkc.destroy_fence (Device.dev device) (Some t) None);
  t

let wait device fences =
  Vkc.wait_for_fences (Vkt.Fence.array fences)
    ~device:(Device.dev device)
    ~wait_all:true
    ~timeout:Unsigned.UInt64.max_int <?> "wait_for_fences"

let reset device fences =
  Vkc.reset_fences ~device:(Device.dev device) (Vkt.Fence.array fences) <?> "reset_fences"
