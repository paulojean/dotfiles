{ pkgs, commonImports, ... }:
(import ./home-shared.nix { inherit pkgs commonImports; })
// {
  imports = commonImports ++ [
    ./programs/rofi.nix
    ./programs/i3
    ./programs/dunst
    ./programs/eww
  ];

  systemd.user.startServices = true;

  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    # (pkgs.nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })
    powerline-fonts
    nerdfonts

    alsa-utils

    fd
    silver-searcher

    plantuml

    aspell
    aspellDicts.en
    aspellDicts.en-computers

    clojure-lsp
  ];

  # xdg.configFile."rofi" = {
  #   recursive = true;
  #   source = ./rofi;
  # };
}
