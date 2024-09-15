{
  lib,
  inputs,
  nixpkgs,
  home-manager,
  user,
  system,
  ...
}:
let
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
  lib = nixpkgs.lib;
in
{
  linux = lib.nixosSystem {
    inherit system;
    specialArgs = {
      inherit inputs user;
    };
    modules = [
      ./laptop
    ];
  };
  wsl = lib.nixosSystem {
    inherit system;
    specialArgs = {
      inherit inputs user;
    };
    modules = [
      inputs.nixos-wsl.nixosModules.default
      {
        system.stateVersion = "24.05";
        wsl = {
          enable = true;
          defaultUser = "paulo";
          useWindowsDriver = true;
          usbip.enable = true;
        };
        nix = {
          extraOptions = ''
            experimental-features = nix-command flakes
          '';
        };

        nix.gc.automatic = true;
        nix.gc.options = "--delete-older-than 10d";
        nixpkgs.config.allowUnfree = true;

        environment.systemPackages = with pkgs; [
          clojure
          babashka
          java

          keychain
          entr

          git
          gitAndTools.diff-so-fancy

          # - tmux-fzf
          bc
        ];
        fonts.packages = with pkgs; [
          powerline-fonts
          font-awesome # icons
          (nerdfonts.override {
            # Nerdfont Icons override
            fonts = [
              "FiraCode"
            ];
          })
        ];

        security.pam.services.gdm.enableGnomeKeyring = true;
        services.gnome.gnome-keyring.enable = true;
      }
    ];
  };
}
