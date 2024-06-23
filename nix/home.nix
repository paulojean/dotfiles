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
  # home = {
  #   username = "paulo";
  #   homeDirectory = "/home/paulo";
  # };
  programs.home-manager = {
    enable = true;
  };
  # home.stateVersion = "23.05";
  home.packages = with pkgs;
    [
      i3
    ];


  xdg.configFile."clojure/deps.edn".text =''
    {:aliases
     {:repl/conjure
        {:extra-deps {nrepl/nrepl       {:mvn/version "1.0.0"}
                      cider/cider-nrepl {:mvn/version "0.42.1"}}
         :main-opts  ["--main" "nrepl.cmdline"
                      "--middleware" "[cider.nrepl/cider-middleware]"
                      "--interactive"]}
      }}
  '';

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
