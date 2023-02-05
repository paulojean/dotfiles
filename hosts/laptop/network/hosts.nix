{ pkgs, ... }:
let
  hosts = pkgs.fetchgit {
    url = "https://github.com/StevenBlack/hosts.git";
    rev = "115afb056ea8da9c16a5ba4face9967160eae828";
    sha256 = "A/35oR/Z1d28RGJyHc3iDfIKeSa7IJec6mB5N/mtCws=";
  };
in
{
  networking.extraHosts = builtins.readFile "${hosts}/hosts";
}
