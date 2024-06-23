{ lib, inputs, nixpkgs, home-manager, user, ... }:

let
  system = "x86_64-linux";
  pkgs = nixpkgs.legacyPackages.${system};
  overlays = [
    inputs.neovim-nightly-overlay.overlays.default
  ];
in
{
  aiur = home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = { inherit inputs user; };
    modules = [
      ./home.nix
      {
        nixpkgs.overlays = overlays;
      }
      {
        home = {
          username = "${user}";
          homeDirectory = "/home/${user}";
          stateVersion = "24.05";
        };
      }
    ];
  };
}
