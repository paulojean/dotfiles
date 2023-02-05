{ pkgs, ... }:
{
  imports = [
    ./programs/bash
    ./programs/git
    ./programs/neovim
    ./programs/emacs
    ./programs/tmux
    ./programs/alacritty
    ./programs/rofi.nix
    ./programs/kitty
    ./programs/i3
    ./programs/dunst
    ./programs/eww
  ];

  systemd.user.startServices = true;
  home = {
    username = "paulo";
    homeDirectory = "/home/paulo";
  };
  programs.home-manager = {
    enable = true;
    path = "~/.config/nixpkgs/home.nix";
  };
  home.stateVersion = "23.05";
  home.packages = with pkgs;
    [
      i3
    ];

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    defaultCommand = ''ag -s --hidden --ignore .git -g ""'';
    fileWidgetOptions = [
      ''
--preview-window wrap --preview '
if [[ -f {} ]]; then
    file --mime {} | grep -q \"text\/.*;\" && bat --color \"always\" {} || (tput setaf 1; file --mime {})
elif [[ -d {} ]]; then
    exa -l --color always {}
else
    tput setaf 1; echo YOU ARE NOT SUPPOSED TO SEE THIS!
fi'
    ''
    ];
  };
}
