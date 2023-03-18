# { pkgs ? import <nixpkgs> {}, ... }:
{ pkgs, ... }:
with pkgs;
{
  nord-vpn = callPackage ./nord-vpn {};
  sxhkd-statusd = callPackage ./sxhkd-statusd {};
  teiler = callPackage ./teiler {};
  nordvpn = callPackage ./nordvpn {};

  icons = {
    pokemon-cursor = callPackage ./icons/pokemon-cursor {};
  };
}
