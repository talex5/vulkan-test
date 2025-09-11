{ pkgs, pkgconf, shaderc, vulkan-headers, vulkan-loader, vulkan-validation-layers, wayland, libffi,
  wayland-protocols, wayland-scanner, libdrm, linuxHeaders }:

pkgs.stdenv.mkDerivation {
  name = "vulkan-test-c";

  src = ./.;

  buildInputs = [
    pkgconf shaderc vulkan-headers vulkan-loader vulkan-validation-layers wayland libffi
    wayland-protocols wayland-scanner libdrm
  ];

  LINUX_HEADERS = linuxHeaders;

  # Use $CC as it allows for stdenv to reference the correct C compiler
  buildPhase = ''make'';
}
