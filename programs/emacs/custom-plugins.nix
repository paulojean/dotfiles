{ pkgs }:
let
  trivialBuild = (pkgs.emacsPackagesFor pkgs.emacs).trivialBuild;
in
{
  emacs-tmux-pane = trivialBuild rec {
    pname = "emacs-tmux-pane";
    version = "20190831";
    src = pkgs.fetchFromGitHub {
      owner = "paulojean";
      repo = pname;
      rev = "70d0dc6fd532568d6d6c803450900e64cf77e0e5";
      sha256 = "K33T9ZbDfrvzrFnYDQBxQL1SgZdX8zEMBfN5bkrdHtg=";
    };
    buildInputs = [
      pkgs.emacs
    ] ++ propagatedUserEnvPkgs;

    propagatedUserEnvPkgs = with pkgs.emacs.pkgs; [
      s dash names
    ];
  };
}
