{ pkgs, pkgconf, shader-slang, vulkan-loader, vulkan-validation-layers, libffi,
  gbm-ocaml, libinput-ocaml, olivine, linuxHeaders, ocamlPackages, fetchFromGitHub }:

let obus =
  let version = "1.2.5"; in
  ocamlPackages.buildDunePackage {
    pname = "obus";
    inherit version;

    src = fetchFromGitHub {
      owner = "ocaml-community";
      repo = "obus";
      tag = version;
      hash = "sha256-Rf79NDhAC1MG8Iyr/V2exTlY6+COKoSSnbkBc8dx/Hg=";
    };

    nativeBuildInputs = [ ocamlPackages.menhir ];

    propagatedBuildInputs = with ocamlPackages; [
      dune_3 ocaml lwt_ppx lwt_react lwt_log xmlm
    ];
  };
in
  pkgs.stdenv.mkDerivation {
    name = "vulkan-test-ocaml";

    src = ./.;

    buildInputs = [
      pkgconf shader-slang vulkan-loader vulkan-validation-layers libffi gbm-ocaml libinput-ocaml olivine obus
    ] ++ (with ocamlPackages; [
      dune_3 ocaml ppx_blob ctypes-foreign ctypes
      fmt eio_main ocamlPackages.wayland
      xmlm lwt # for obus
    ]);

    LINUX_HEADERS = linuxHeaders;
    buildPhase = ''make'';
  }
