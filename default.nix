{ pkgs, pkgconf, shaderc, vulkan-loader, vulkan-validation-layers, libffi, libdrm, linuxHeaders, ocamlPackages }:

pkgs.stdenv.mkDerivation {
  name = "vulkan-test-ocaml";

  src = ./.;

  buildInputs = [
    pkgconf shaderc vulkan-loader vulkan-validation-layers libffi libdrm
  ] ++ (with ocamlPackages; [
    dune_3 dune-configurator ocaml ppxlib integers ppx_blob findlib ctypes-foreign ctypes
    xmlm fmt menhir eio_main ocamlPackages.wayland
  ]);

  LINUX_HEADERS = linuxHeaders;
  buildPhase = ''make'';
}
