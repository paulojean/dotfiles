{ pkgs, ... }:
{
  imports = [
    ./programs/bash
    ./programs/git
    ./programs/neovim
    ./programs/emacs
    ./programs/tmux
    ./programs/alacritty
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
  home.stateVersion = "22.05";
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

  xdg.configFile."dunst/dunstrc".source = ./dunst/dunstrc;

  xdg.configFile."i3" = {
    recursive = true;
    source = ./i3;
  };

  xdg.configFile."i3blocks-contrib" = {
    recursive = true;
    source = ./i3blocks-contrib;
  };

  xdg.configFile."i3blocks" = {
    recursive = true;
    source = ./i3blocks;
  };

  xdg.configFile."rofi" = {
    recursive = true;
    source = ./rofi;
  };

  programs.eww = {
    enable = true;
    configDir = ./eww;
  };

  programs.kitty = {
    enable = true;
    extraConfig = builtins.readFile ./kitty/kitty.conf;
  };
}
