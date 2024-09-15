{
  description = "home";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-wsl.url = "github:nix-community/NixOS-WSL/main";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    darwin = {
      # MacOS Package Management
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      nixpkgs,
      home-manager,
      darwin,
      ...
    }:
    let
      systemMacos = "aarch64-darwin";
      systemLinux = "x86_64-linux";
      user = "paulo";
      commonInherits = {
        inherit (nixpkgs) lib;
        inherit
          inputs
          nixpkgs
          home-manager
          user
          ;
      };
      mkHomeConfiguration =
        { system, homeDirectory }:
        (import ./nix (
          commonInherits
          // {
            inherit system homeDirectory;
          }
        ));
      mkNixosConfiguration =
        { }:
        (import ./hosts {
          inherit (nixpkgs) lib;
          inherit
            inputs
            nixpkgs
            home-manager
            user
            ;
          system = systemLinux;
        });
    in
    {
      nixosConfigurations = {
        wsl = (mkNixosConfiguration { }).wsl;
        linux = (mkNixosConfiguration { }).linux;
      };
      darwinConfigurations = (
        # Darwin Configurations
        import ./darwin {
          inherit (nixpkgs) lib;
          inherit
            inputs
            nixpkgs
            home-manager
            darwin
            ;
        }
      );

      homeConfigurations = {
        # macos
        aiur =
          (mkHomeConfiguration {
            system = systemMacos;
            homeDirectory = "/Users/${user}";
          }).macos;
        # linux
        valinor =
          (mkHomeConfiguration {
            system = systemLinux;
            homeDirectory = "/home/${user}";
          }).linux;
      };
    };
}
