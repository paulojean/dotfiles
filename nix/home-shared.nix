{ pkgs, ... }:
{
  programs.home-manager = {
    enable = true;
  };

  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    powerline-fonts
    nerdfonts

    fd
    silver-searcher

    plantuml

    aspell
    aspellDicts.en
    aspellDicts.en-computers

    clojure-lsp
  ];

  xdg.configFile."lsp/config.edn".text = "
    {:clean {:ns-inner-blocks-indentation :same-line
             :sort                        {:refer {:max-line-length 160}}}}";

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    autosuggestion.highlight = "fg=#ff00ff,bg=cyan,bold,underline";
    enableVteIntegration = true;
    autocd = true;
    # cdpath = true;

    syntaxHighlighting.enable = true;

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
      lt = "ls -lT";
      vi = "nvim";
      vim = "nvim";
      n = "nvim";
      e = "emacsclient --alternate-editor \"\" --tty";
      dev = "tmux -2 new-session -A -s stuffs";
    };
  };

  # xdg.configFile."rofi" = {
  #   recursive = true;
  #   source = ./rofi;
  # };

  xdg.configFile."clojure/deps.edn".text = ''
    {:aliases
     {:repl/conjure
        {:extra-deps {nrepl/nrepl       {:mvn/version "1.0.0"}
                      cider/cider-nrepl {:mvn/version "0.42.1"}}
         :main-opts  ["--main" "nrepl.cmdline"
                      "--middleware" "[cider.nrepl/cider-middleware]"
                      "--interactive"]}
      }}
  '';
  xdg.configFile."clojure-lsp/config.edn".text = ''
    {:clean {:ns-inner-blocks-indentation :same-line
     :sort                        {:refer {:max-line-length 160}}}}
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
