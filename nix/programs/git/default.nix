{ pkgs, ... }:
{
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
}
