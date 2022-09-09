{ pkgs, ... }: with pkgs;
let
  suggestions.bash = import ./suggestions.nix {};
in
{
  programs.readline = {
    enable = true;
    extraConfig = ''
      # Pressing tab will list all completions & select the first one. Pressing it
      # again will cycle through available completions.
      TAB: menu-complete
      # Shift-TAB cycles completions backward
      "\e[Z": menu-complete-backward
    '';
    variables = {
      show-mode-in-prompt =  "on";
      vi-ins-mode-string = ''\1\e[34;1m\2(insert)\1\e[0m\2'';
      vi-cmd-mode-string = ''\1\e[35;1m\2(normal)\1\e[0m\2'';
      editing-mode = "vi";

      # Show all completions as soon as I press tab, even if there's more than one
      show-all-if-ambiguous = "on";
      # Ignore case
      completion-ignore-case = "on";
      # on menu-complete, first display the common prefix, then cycle through the
      # options when hitting TAB
      menu-complete-display-prefix = "on";
      # Keymaps for when we're in insert (i.e., typing stuff in) mode
      keymap = "vi-insert";
      # Remove delay when switching modes. Ref: https://superuser.com/a/1161871/586817
      keyseq-timeout = "0.01";
    };
  };

  programs.bash = {
    enable = true;
    profileExtra = ''[[ -f ~/.bashrc ]] && . ~/.bashrc'';
    initExtra = (builtins.readFile ./.bashrc) + ''
      [ -f ${pkgs.bash-preexec}/share/bash/bash-preexec.sh ] && . ${pkgs.bash-preexec}/share/bash/bash-preexec.sh
      [ -f ${suggestions.bash}/share/bash/suggestions.sh ] && . ${suggestions.bash}/share/bash/suggestions.sh
    '';
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
}
