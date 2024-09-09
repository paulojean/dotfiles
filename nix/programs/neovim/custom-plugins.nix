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
      rev = "6d2bc7f7b24c2c43d54f263bee7b9b08aef5d1a1";
      sha256 = "sha256-cxA9kzio1bxisse+6TVgxKuajmOWykvL1ZGjz8gS0z0=";
    };
  };
  cmp-conjure = pkgs.vimUtils.buildVimPlugin {
    name = "cmp-conjure";
    src = pkgs.fetchFromGitHub {
      owner = "PaterJason";
      repo = "cmp-conjure";
      rev = "8c9a88efedc0e5bf3165baa6af8a407afe29daf6";
      sha256 = "sha256-uTiXG8p0Cqc4o45ckscRSSv0qGqbfwuryqWBZHEh8Mc=";
    };
  };
  nvim-treesitter = pkgs.vimUtils.buildVimPlugin {
    name = "nvim-treesitter";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-treesitter";
      repo = "nvim-treesitter";
      rev = "c436d45eeeeb78e5482cb28b59de1d7a77c93d86";
      sha256 = "sha256-NknG9Log3GMU67yAoSrs4rX5JEV+wCssPA6VYxoFrHg=";
    };
  };
  nvim-treesitter-context = pkgs.vimUtils.buildVimPlugin {
    name = "nvim-treesitter-context";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-treesitter";
      repo = "nvim-treesitter-context";
      rev = "e6cc783b74606d97ca9eff6494e3f5c2ca603a50";
      sha256 = "sha256-wDRd6qFbfHYwWNCfc7Ixg/aUkGUVRTt/TEBvrTn+IEg=";
    };
  };
  nvim-treesitter-textobjects = pkgs.vimUtils.buildVimPlugin {
    name = "nvim-treesitter-textobjects";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-treesitter";
      repo = "nvim-treesitter-textobjects";
      rev = "bf8d2ad35d1d1a687eae6c065c3d524f7ab61b23";
      sha256 = "sha256-qDk74rGr9uO/WdSoMZMr8i4bxjlkGDRwpJKaQA+MdgE=";
    };
  };
  nvim-treesitter-sexp = pkgs.vimUtils.buildVimPlugin {
    name = "nvim-treesitter-sexp";
    src = pkgs.fetchFromGitHub {
      owner = "PaterJason";
      repo = "nvim-treesitter-sexp";
      rev = "32509f4071f9c8ba5655bf2e1ccf1f1cd8447da0";
      sha256 = "sha256-ehpGvHnY28Ym55B7ituwcvZmGmLt1x92J5M+m8j1ytU=";
    };
  };
  # https://gitlab.com/HiPhish/rainbow-delimiters.nvim
  rainbow-delimiters = pkgs.vimUtils.buildVimPlugin {
    name = "rainbow-delimiters";
    src = pkgs.fetchFromGitLab {
      owner = "HiPhish";
      repo = "rainbow-delimiters.nvim";
      rev = "5f73b24aeb94f5274c218955573153c69ce4d1ee";
      sha256 = "0qwlq6h2skpppn2aai7d9qhfk6cwfl8zz3lj7llyid0y8qqig41z";
    };
  };
  nvim-listchars = pkgs.vimUtils.buildVimPlugin {
    name = "nvim-listchars";
    src = pkgs.fetchFromGitLab {
      owner = "0xfraso";
      repo = "nvim-listchars";
      rev = "40b05e8375af11253434376154a9e6b3e9400747";
      sha256 = "sha256-SQPe1c3EzVdqpU41FqwR2owfstDqSLjNlrpJuaLZXNE=";
    };
  };
  coc-clojure = pkgs.vimUtils.buildVimPlugin {
    name = "coc-clojure";
    src = pkgs.fetchFromGitHub {
      owner = "NoahTheDuke";
      repo = "coc-clojure";
      rev = "effa179688234e91526249f4ffd2163e91271014";
      sha256 = "sha256-z+wp+MfCzaA8l8RHK8vo1EMcrQnQVrDVbP+DqvrBMOk=";
    };
  };
}
