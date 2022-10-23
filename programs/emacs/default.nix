{ pkgs, ... }:
let
  plugins = pkgs.callPackage ./custom-plugins.nix {};
in
{
  services.emacs.enable = true;
  programs.emacs = {
    enable = true;

    extraConfig = builtins.readFile ./init.el;
    extraPackages = epkgs:
    (with epkgs; [
      magit
      forge

      evil
      evil-collection
      evil-leader
      evil-surround
      evil-org
      evil-commentary
      evil-easymotion

      ivy
      flyspell-correct-ivy

      org-bullets

      undo-tree
      ag
      dumb-jump
      google-c-style

      ibuffer-projectile
      projectile
      counsel-projectile

      keychain-environment

      company
      company-box
      prescient

      comment-tags
      flycheck
      flycheck-popup-tip
      flycheck-joker
      flycheck-checkbashisms

      flx
      flx-ido

      ranger

      highlight-parentheses

      gruvbox-theme
      spaceline
      linum-relative
      which-key
      key-chord
      command-log-mode
      eyebrowse
      restclient

      # programming
      yaml-mode
      clojure-mode
      clojure-mode-extra-font-locking
      clj-refactor
      go-mode
      nix-mode
      haskell-mode
      lua-mode
      feature-mode
      markdown-mode
      groovy-mode
      dockerfile-mode
      scala-mode
      sbt-mode
      vimrc-mode
      cider

      # editing
      paredit

      # lsp
      lsp-mode
      lsp-java
      lsp-metals
      dap-mode
      lsp-ui
      lsp-ivy


      plugins.emacs-tmux-pane

    ]);
  };
}
