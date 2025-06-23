{
  description = "Testing opengl";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
  };

  outputs = { self, nixpkgs }:
  let pkgs = nixpkgs.legacyPackages.x86_64-linux; in {
    packages.x86_64-linux.default = pkgs.callPackage (import ./default.nix) {};
  };
}
