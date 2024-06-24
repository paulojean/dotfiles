{ lib, inputs, nixpkgs, home-manager, user, ... }:

let
  system = "aarch64-darwin";
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
          homeDirectory = "/Users/${user}";
          stateVersion = "24.05";
        };
      }
    ];
  };
}
