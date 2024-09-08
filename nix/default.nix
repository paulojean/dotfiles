{
  lib,
  inputs,
  nixpkgs,
  home-manager,
  user,
  system,
  homeDirectory,
  ...
}:
let
  pkgs = nixpkgs.legacyPackages.${system};
  overlays = [
    inputs.neovim-nightly-overlay.overlays.default
  ];
  commonImports = [
    ./programs/bash
    ./programs/git
    ./programs/emacs
    ./programs/tmux
    ./programs/alacritty
    ./programs/kitty
    (import ./programs/neovim {
      inherit lib pkgs;
      nvim-package = inputs.neovim-nightly-overlay.packages.${system}.neovim;
    })
  ];
in
{
  macos = home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = {
      inherit inputs user;
    };
    modules = [
      (import ./home-macos.nix { inherit pkgs commonImports; })
      {
        nixpkgs.overlays = overlays;
      }
      {
        home = {
          username = "${user}";
          homeDirectory = homeDirectory;
          stateVersion = "24.11";
        };
      }
    ];
  };
  linux = home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = {
      inherit inputs user;
    };
    modules = [
      {
        nixpkgs.overlays = [ inputs.neovim-nightly-overlay.overlays.default ];
        home = {
          username = "${user}";
          homeDirectory = "/home/${user}";
          stateVersion = "24.11";
        };
      }
      (import ./home-linux.nix { inherit pkgs commonImports; })
    ];
  };
}
