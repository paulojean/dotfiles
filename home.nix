{ pkgs, ... }:

{
  systemd.user.startServices = true;
  home = {
    username = "paulo";
    homeDirectory = "/home/paulo";
 };
  programs.home-manager = {
    enable = true;
    path = "~/.config/nixpkgs/home.nix";
  };
  home.stateVersion = "20.09";
  home.packages = with pkgs;
  [
    pkgs.bspwm
    pkgs.sxhkd
    pkgs.i3
  ];

  programs.bash = {
    enable = true;
    profileExtra = ''[[ -f ~/.bashrc ]] && . ~/.bashrc'';
    initExtra = builtins.readFile ./.bashrc;
    shellAliases = {
      ls="exa --group-directories-first";
      ll="exa -la --group-directories-first";
      lt="exa -lT --git-ignore";
      adbsi="adb shell input text";
      ipup="sudo ip link set wlp2s0 up";
      g = "git";
      vi = "nvim";
      vim = "nvim";
      n = "nvim";
      e = "emacsclient --alternate-editor \"\" --tty";
      die = "shutdown now";
      fuck = "sudo \"$SHELL\" -c \"$(history -p !!)\"";
      dev = "tmux -2 new-session -A -s stuffs";
      pipup = "pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U";
      pipup3 = "pip3.7 list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip3.7 install -U";
      du = "ncdu --color dark -rr -x --exclude .git --exclude node_modules";
      clear-clipboard = "printf '' | xclip -selection clipboard";
      ".."="cd ..";
      "..."="cd ../..";
      "...."="cd ../../..";
      "....."="cd ../../../..";
      "......"="cd ../../../../..";
    };
  };

  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "Paulo Sousa";
    userEmail = "pauloj10" + "@" + "gmail.com";
    aliases = {
      br = "branch";
      bd = "branch -D";
      df = "diff";
      st = "status";
      co = "checkout";
      cb = "checkout -b";
      ci = "commit";
      cm = "commit -m";
      dc = "diff --cached";
      lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%an, %cr)%Creset' --abbrev-commit --date=relative";
      aad = "add .";
      comend = "commit --amend --no-edit";
      please = "push --force-with-lease";
      rehead = "reset HEAD~";
    };
    extraConfig = {
      core = {
        editor = "nvim";
        pager = "diff-so-fancy | less --tabs=4 -RFX";
      };
      push = {
        default = "current";
      };
    };
    ignores = [
      "*.elc"
      ".*.class"
      ".cache/"
      ".cask/"
      ".cpcache"
      ".dumbjump"
      ".emacs.d/bookmarks"
      ".emacs.d/forge-database.sqlite"
      ".emacs.d/recentf"
      ".emacs.d/transient"
      ".emacs.d/url"
      ".dir-locals.el"
      ".netrwhist"
      ".nrepl-port"
      ".projectile"
      ".tags"
      ".log"
      ".lsp"
      ".lsp/"
      "eshell/"
      "fonts/"
      "pom.xml"
      "projectile.cache"
      "target/"
      ".emacs.d/.lsp-session-*"
      ".emacs.d/history"
      ".emacs.d/workspace/"
    ];
  };

  xdg.configFile."dunst/dunstrc".source = ./dunst/dunstrc;

  xdg.configFile."sxhkd" = {
    recursive = true;
    source = ./sxhkd;
  };

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

  xdg.configFile."nvim" = {
    recursive = true;
    source = ./nvim;
  };

  xdg.configFile."polybar" = {
    recursive = true;
    source = ./polybar;
  };

  xdg.configFile."rofi" = {
    recursive = true;
    source = ./rofi;
  };

  programs.kitty = {
    enable = true;
    extraConfig = builtins.readFile ./kitty/kitty.conf;
  };

  # xsession = {
  #   enable = true;
  #   windowManager.i3 = {
  #     enable = true;
  #     extraConfig = builtins.readFile ./i3/config;
  #   };
  #   # windowManager.bspwm = {
  #   #   enable = true;
  #   #   extraConfig = builtins.readFile ./bspwm/bspwmrc;
  #   #   # monitors = {
  #   #     #   eDP-1 = ["1" "2" "3" "4" "5" "6" "7" "8" "9" "10"];
  #   #     # };
  #   #     # settings = {
  #   #       #   border_width = 1;
  #   #       #   window_gap = 0;
  #   #       #   split_ratio = 0.52;
  #   #       #   borderless_monocle = true;
  #   #       #   gapless_monocle = true;
  #   #       # };
  #   #       # rules = {
  #   #         #   "Yad" = {
  #   #           #     state = "floating";
  #   #           #   };
  #   #           # };
  #   # };
  # };
}
