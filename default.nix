{ pkgs, pkgconf, shader-slang, vulkan-loader, vulkan-validation-layers, libffi, libdrm, linuxHeaders, ocamlPackages }:

pkgs.stdenv.mkDerivation {
  name = "vulkan-test-ocaml";

  src = ./.;

  buildInputs = [
    pkgconf shader-slang vulkan-loader vulkan-validation-layers libffi libdrm
  ] ++ (with ocamlPackages; [
    dune_3 dune-configurator ocaml ppxlib integers ppx_blob findlib ctypes-foreign ctypes
    xmlm fmt menhir eio_main ocamlPackages.wayland cairo2
  ]);

  LINUX_HEADERS = linuxHeaders;
  buildPhase = ''make'';
}
