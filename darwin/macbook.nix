{ config, pkgs, vars, ... }:

{
  imports = [
    # ./modules/skhd.nix
    # ./modules/yabai.nix
    # ../nix/programs/tmux
  ];

  users.users.${vars.user} = {
    home = "/Users/${vars.user}";
    shell = pkgs.zsh;                     # Default Shell
  };

  networking = {
    computerName = "MGC2WT66YC";             # Host Name
    hostName = "MGC2WT66YC";
  };
  services.skhd = {
    enable = true;
    skhdConfig = ''
cmd - return : /Applications/Kitty.app/Contents/MacOS/kitty --single-instance -d ~

# focus window
cmd - h : yabai -m window --focus west
cmd - j : yabai -m window --focus south
cmd - k : yabai -m window --focus north
cmd - l : yabai -m window --focus east

#  `;` key
cmd - 0x29 : yabai -m window --focus "$(yabai -m query --windows | jq -re "sort_by(.display, .frame.x, .frame.y, .id) | reverse | map(select(.visible == 1 and .subrole != \"AXUnknown\")) | reverse | nth(index(map(select(.focused == 1))) - 1).id")"
# `'` key
cmd - 0x27 : yabai -m window --focus "$(yabai -m query --windows | jq -re "sort_by(.display, .frame.x, .frame.y, .id) | map(select(.visible == 1 and .subrole != \"AXUnknown\")) | reverse | nth(index(map(select(.focused == 1))) - 1).id")"


    '';
  };


  programs.tmux = {
    enable = true;
    enableFzf = true;
    enableSensible = true;
    enableVim = true;
    iterm2 = true;
    # extraConfig = builtins.readFile ../nix/programs/tmux/
  };

  fonts = {                               # Fonts
    fontDir.enable = true;
    fonts = with pkgs; [
      source-code-pro
      font-awesome
      nerdfonts
      # (nerdfonts.override {
      #   fonts = [
      #     "FiraCode"
      #   ];
      # })
    ];
  };

  environment = {
    shells = with pkgs; [ zsh ];          # Default Shell
    variables = {                         # Environment Variables
      EDITOR = "${vars.editor}";
      VISUAL = "${vars.editor}";
    };
    systemPackages = with pkgs; [         # System-Wide Packages
      # Terminal
      git
      pfetch
      ranger

      emacs
      fd

      silver-searcher

      clojure-lsp
    ];
  };

  programs = {
    zsh.enable = true;                    # Shell
  };

  services = {
    nix-daemon.enable = true;             # Auto-Upgrade Daemon
  };

  homebrew = {                            # Homebrew Package Manager
    enable = true;
    onActivation = {
      autoUpdate = false;
      upgrade = false;
      cleanup = "zap";
    };
    brews = [
      "wireguard-tools"
    ];
    casks = [
      "moonlight"
      "plex-media-player"
    ];
  };

  nix = {
    package = pkgs.nix;
    gc = {                                # Garbage Collection
      automatic = true;
      interval.Day = 7;
      options = "--delete-older-than 7d";
    };
    extraOptions = ''
      auto-optimise-store = true
      experimental-features = nix-command flakes
    '';
  };

  system = {                              # Global macOS System Settings
    defaults = {
      NSGlobalDomain = {
        KeyRepeat = 1;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
      };
      dock = {
        autohide = true;
        orientation = "bottom";
        showhidden = true;
        tilesize = 40;
      };
      finder = {
        QuitMenuItem = false;
      };
      trackpad = {
        Clicking = true;
        TrackpadRightClick = true;
      };
    };
    activationScripts.postActivation.text = ''sudo chsh -s ${pkgs.zsh}/bin/zsh''; # Set Default Shell
    stateVersion = 4;
  };

  home-manager.users.${vars.user} = {
    home = {
      stateVersion = "23.11";
    };

    imports = [
      ../nix/programs/neovim
      ../nix/programs/kitty
    ];

    programs.zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableSyntaxHighlighting = true;
      enableCompletion = true;
      enableVteIntegration = true;
      autocd = true;
      # cdpath = true;

      dotDir = ".config/zsh";

      history = {
        expireDuplicatesFirst = true;
        extended = true;
        ignoreSpace = true;
        save = 50000;
        size = 50000;
      };

      prezto = {
        enable = true;
        editor.keymap = "vi";
        editor.promptContext = true;

        prompt.theme = "powerlevel10k";
        # pmodules = [
        #   "git"
        # ];
      };

      localVariables = {
        POWERLEVEL9K_LEFT_PROMPT_ELEMENTS = [
          "dir"
          "vcs"
          "vi_mode"
        ];

        # POWERLEVEL9K_VI_NORMAL_MODE_STRING = "(n)";
        # POWERLEVEL9K_VI_VISUAL_MODE_STRING = "(v)";
        # POWERLEVEL9K_VI_MODE_VISUAL_BACKGROUND = "grey";
        # POWERLEVEL9K_VI_MODE_VISUAL_FOREGROUND = "yellow";
        # POWERLEVEL9K_VI_INSERT_MODE_STRING = "(i)";
        # POWERLEVEL9K_VI_MODE_INSERT_BACKGROUND = "grey";
        # POWERLEVEL9K_VI_MODE_INSERT_FOREGROUND = "red";


      };

      shellAliases = {
        ll = "ls -l";
        lt="ls -lT";
        vi = "nvim";
        vim = "nvim";
        n = "nvim";
        e = "emacsclient --alternate-editor \"\" --tty";
        dev = "tmux -2 new-session -A -s stuffs";
      };
    };
  };
}
