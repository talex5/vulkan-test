module A = Vulkan.A

let memcpy_unsafe = Ctypes.(Foreign.foreign "memcpy" (ptr void @-> ptr void @-> size_t @-> returning void))

let ptr arr = Ctypes.to_voidp (A.start arr)

let copy ~dst src =
  let len_src = A.length src * Ctypes.sizeof (A.element_type src) in
  let len_dst = A.length dst * Ctypes.sizeof (A.element_type dst) in
  assert (len_src <= len_dst);
  memcpy_unsafe (ptr dst) (ptr src) (Unsigned.Size_t.of_int len_src)
