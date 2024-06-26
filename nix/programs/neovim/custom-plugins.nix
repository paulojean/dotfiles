{ pkgs, ... }:

{
  nvim-contabs = pkgs.vimUtils.buildVimPlugin {
    name = "nvim-contabs";
    src = pkgs.fetchFromGitHub {
      owner = "m00qek";
      repo = "nvim-contabs";
      rev = "beb26832af60cd3747295b779469257d1a815c9a";
      sha256 = "1xlchkx754ajag4mp03bi2jdhdq7ya2a7hnvbpgykb9xpx71cl0y";
    };
  };
  nvim-tmux-navigation = pkgs.vimUtils.buildVimPlugin {
    name = "nvim-tmux-navigation";
    src = pkgs.fetchFromGitHub {
      owner = "alexghergh";
      repo = "nvim-tmux-navigation";
      rev = "4898c98702954439233fdaf764c39636681e2861";
      sha256 = "sha256-CxAgQSbOrg/SsQXupwCv8cyZXIB7tkWO+Y6FDtoR8xk=";
    };
  };
  conjure = pkgs.vimUtils.buildVimPlugin {
    name = "conjure";
    src = pkgs.fetchFromGitHub {
      owner = "Olical";
      repo = "conjure";
      rev = "a81977726f726b21d4bd75cea5bbbb7022005507";
      sha256 = "sha256-Y+RC8XvRYfNEUePQysYYC5E7mameeBS9VCerzgSci5Q=";
    };
  };
  cmp-conjure = pkgs.vimUtils.buildVimPlugin {
    name = "cmp-conjure";
    src = pkgs.fetchFromGitHub {
      owner = "PaterJason";
      repo = "cmp-conjure";
      rev = "sha256-uTiXG8p0Cqc4o45ckscRSSv0qGqbfwuryqWBZHEh8Mc=";
      sha256 = "sha256-uTiXG8p0Cqc4o45ckscRSSv0qGqbfwuryqWBZHEh8Mc=";
    };
  };
  nvim-treesitter-sexp = pkgs.vimUtils.buildVimPlugin {
    name = "nvim-treesitter-sexp";
    src = pkgs.fetchFromGitHub {
      owner = "PaterJason";
      repo = "cmp-conjure";
      rev = "32509f4071f9c8ba5655bf2e1ccf1f1cd8447da0";
      sha256 = "sha256-ehpGvHnY28Ym55B7ituwcvZmGmLt1x92J5M+m8j1ytU=";
    };
  };
}
