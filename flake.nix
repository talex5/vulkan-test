{
  description = "Testing opengl";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
    libdrm-ocaml = {
      url = "github:talex5/libdrm-ocaml/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    eio-trace = {
      url = "github:ocaml-multicore/eio-trace";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, eio-trace, libdrm-ocaml }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    packages.${system}.default = pkgs.callPackage (import ./default.nix) {
      libdrm-ocaml = libdrm-ocaml.packages.${system}.default;
    };

    devShells.${system}.default = pkgs.mkShell {
      MERLIN = pkgs.ocamlPackages.merlin;
      OCPINDENT = pkgs.ocamlPackages.ocp-indent;
      VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";

      buildInputs = self.outputs.packages.${system}.default.buildInputs;
      packages = [ pkgs.ocamlPackages.ocp-indent eio-trace.packages.${system}.default ];
      shellHook = ''exec ${pkgs.fish}/bin/fish'';
    };
  };
}
