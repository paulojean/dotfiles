{
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
    ./programs/neovim
    ./programs/emacs
    ./programs/tmux
    ./programs/alacritty
    ./programs/kitty
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
          stateVersion = "24.05";
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
      (import ./home-linux.nix { inherit commonImports; })
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
