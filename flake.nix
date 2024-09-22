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

    # nvim packages
    cmp-conjure.flake = false;
    cmp-conjure.url = "github:PaterJason/cmp-conjure";
    conjure.flake = false;
    conjure.url = "github:Olical/conjure";
    nvim-tmux-navigation.flake = false;
    nvim-tmux-navigation.url = "github:alexghergh/nvim-tmux-navigation";
    nvim-treesitter-context.flake = false;
    nvim-treesitter-context.url = "github:nvim-treesitter/nvim-treesitter-context";
    nvim-treesitter-sexp.flake = false;
    nvim-treesitter-sexp.url = "github:PaterJason/nvim-treesitter-sexp";
    nvim-treesitter-textobjects.flake = false;
    nvim-treesitter-textobjects.url = "github:nvim-treesitter/nvim-treesitter-textobjects";
    nvim-treesitter.flake = false;
    nvim-treesitter.url = "github:nvim-treesitter/nvim-treesitter";
  };

  outputs = inputs @ {
    nixpkgs,
    home-manager,
    darwin,
    ...
  }: let
    systemMacos = "aarch64-darwin";
    systemLinux = "x86_64-linux";
    user = "paulo";
    nvim-plugins = {
      cmp-conjure = inputs.cmp-conjure;
      conjure = inputs.conjure;
      nvim-tmux-navigation = inputs.nvim-tmux-navigation;
      nvim-treesitter = inputs.nvim-treesitter;
      nvim-treesitter-context = inputs.nvim-treesitter-context;
      nvim-treesitter-sexp = inputs.nvim-treesitter-sexp;
      nvim-treesitter-textobjects = inputs.nvim-treesitter-textobjects;
    };
    commonInherits = {
      inherit (nixpkgs) lib;
      inherit
        inputs
        nixpkgs
        home-manager
        user
        nvim-plugins
        ;
    };
    mkHomeConfiguration = {
      system,
      homeDirectory,
    }: (import ./nix (
      commonInherits
      // {
        inherit system homeDirectory;
      }
    ));
    mkNixosConfiguration = {}: (import ./hosts {
      inherit (nixpkgs) lib;
      inherit
        inputs
        nixpkgs
        home-manager
        user
        ;
      system = systemLinux;
    });
  in {
    nixosConfigurations = {
      wsl = (mkNixosConfiguration {}).wsl;
      linux = (mkNixosConfiguration {}).linux;
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
        })
        .macos;
      # linux
      valinor =
        (mkHomeConfiguration {
          system = systemLinux;
          homeDirectory = "/home/${user}";
        })
        .linux;
    };
  };
}
