{
  description = "Testing opengl";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
  };

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system}; in {
    packages.${system}.default = pkgs.callPackage (import ./default.nix) {};

    devShells.${system}.default = pkgs.mkShell {
      VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";

      buildInputs = self.outputs.packages.${system}.default.buildInputs;
      shellHook = ''exec ${pkgs.fish}/bin/fish'';
    };
  };
}
