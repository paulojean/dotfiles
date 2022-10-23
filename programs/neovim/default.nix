{ pkgs, ... }:
let
  plugins = pkgs.vimPlugins // pkgs.callPackage ./custom-plugins.nix {};
in {
  programs.neovim = {
    enable = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withRuby = true;
    withPython3 = true;

    plugins =  with plugins; [
      fzf-vim
      undotree
      ncm2
      ack-vim
      vim-easymotion

      ale
      lightline-vim
      lightline-ale
      lightline-bufferline

      vim-tmux
      tmux-navigator

      nvim-contabs

      # ui
      rainbow_parentheses
      gruvbox-nvim

    ];
    extraConfig = builtins.concatStringsSep "\n" [
      ''
        lua << EOF
        ${pkgs.lib.strings.fileContents ./config.lua}
        EOF
      ''

      (pkgs.lib.strings.fileContents ./config.vim)
    ];
  };
}
