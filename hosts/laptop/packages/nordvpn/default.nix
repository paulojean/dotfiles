{ pkgs ? import <nixpkgs> {}, ... }: with pkgs;
# with (import <nixpkgs> {}).pkgs;
let
  nordvpn = stdenv.mkDerivation rec {
    pname = "nordvpn";
    version = "3.15.0";

    src = fetchurl {
      url = "https://repo.nordvpn.com/deb/nordvpn/debian/pool/main/${pname}_${version}_amd64.deb";
      sha256 = "sha256-+ulpnAcxFXCF1kxuh5S6NMlRW/RvK5QgjfccOEKrtok=";
    };

    nativeBuildInputs = [autoPatchelfHook dpkg];

    buildInputs = [libidn2 libxml2 zlib];

    unpackPhase = ''
      dpkg-deb -x $src $out
    '';
    installPhase = ''
      mv $out/usr/* $out
      rmdir $out/usr

      for f in "$out/lib/systemd/system/nordvpnd.service" \
                "$out/etc/init.d/nordvpn"; do
        substituteInPlace $f --replace "/usr/sbin" "$out/sbin"
      done

      patchShebangs $out/share/bash-completion/completions/nordvpn
      # substituteInPlace $out/share/bash-completion/completions/nordvpn \
      #   --replace "/bin/bash" "/usr/bin/env bash"
    '';
  };
in nordvpn
# buildFHSUserEnv {
#   name = "nordvpnd";
#   targetPkgs = pkgs: [
#     nordvpn
#     libidn2 libxml2 zlib
#   ];
#   runScript = "${nordvpn}/usr/sbin/nordvpnd";
# }
