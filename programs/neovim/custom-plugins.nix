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
}
