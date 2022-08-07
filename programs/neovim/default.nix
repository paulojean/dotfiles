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

      ale
      lightline-ale
      lightline-bufferline

      vim-tmux
      tmux-navigator

      rainbow_parentheses

      nvim-contabs
    ];
    extraConfig = builtins.readFile ./config.vim;
  };
}
