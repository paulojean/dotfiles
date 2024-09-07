{ pkgs, commonImports, ... }:
(import ./home-shared.nix { inherit pkgs; })
// {
  imports = commonImports;
}
