{ pkgs ? import <nixpkgs> {}, ... }: with pkgs;
stdenv.mkDerivation rec {
  pname = "suggestions.bash";
  version = "0.0.1";
  src = fetchFromGitHub {
    owner = "paulojean";
    repo = "suggestions.bash";
    rev = "951779831acac3501ffdf6c34334a60dc4ae7d88";
    sha256 = "0l235jkml8lgi97dx5pz3v1a1wi1k0z9chqq6bk8a8cxk0gj01g4";
  };
  installPhase = ''
    mkdir -p $out/share/bash
    cp .suggestions.sh $out/share/bash/suggestions.sh
  '';
}
