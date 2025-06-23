{ pkgs, glfw3, pkgconf, shaderc, vulkan-headers, vulkan-loader, vulkan-validation-layers, wayland, libffi,
  wayland-protocols, wayland-scanner, libdrm, linuxHeaders }:

pkgs.stdenv.mkDerivation {
  name = "hello-3d";

  src = ./.;

  buildInputs = [
    pkgconf glfw3 shaderc vulkan-headers vulkan-loader vulkan-validation-layers wayland libffi
    wayland-protocols wayland-scanner libdrm
  ];

  LINUX_HEADERS = linuxHeaders;

  VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";

  # Use $CC as it allows for stdenv to reference the correct C compiler
  buildPhase = ''make'';
}
