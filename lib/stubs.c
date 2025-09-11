#include <vulkan/vulkan.h>
#include <caml/mlvalues.h>

// This isn't actually used; it's just so the linker knows we're using libvulkan.
value caml_ensure_vulkan_linked(value v_unit) {
  uint32_t extensionCount = 0;
  vkEnumerateInstanceExtensionProperties(NULL, &extensionCount, NULL);
  return Val_unit;
}
