{ config, pkgs, user, ... }:
let
  packages = pkgs.callPackage ./packages { inherit pkgs; } ;
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./network/hosts.nix
    ];

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 10d";

  nixpkgs.config.allowUnfree = true;

  hardware.cpu.intel.updateMicrocode = true;
  hardware.bluetooth = {
    enable = true;
    settings = {
      General = { Enable = "Source,Sink,Media,Socket"; };
    };
  };

  # ## pipewire start
  # security.rtkit.enable = true;
  # services.pipewire = {
  #   enable = true;
  #   alsa.enable = true;
  #   alsa.support32Bit = true;
  #   pulse.enable = true;
  #   jack.enable = true;
  # };
  # environment.etc = {
  #   "wireplumber/bluetooth.lua.d/51-bluez-config.lua".text = ''
  #   bluez_monitor.properties = {
  #     ["bluez5.enable-sbc-xq"] = true,
  #     ["bluez5.enable-msbc"] = true,
  #     ["bluez5.enable-hw-volume"] = true,
  #     ["bluez5.headset-roles"] = "[ hsp_hs hsp_ag hfp_hf hfp_ag ]"
  #                                             }
  # '';
  # };
  # hardware.pulseaudio = {
  #   enable = true;
  #   # NixOS allows either a lightweight build (default) or full build of PulseAudio to be installed.
  #   # Only the full build has Bluetooth support, so it must be selected here.
  #   package = pkgs.pulseaudioFull;
  # }
  # ## pipewire end

  # hardware.nvidia.prime = {
  #   offload.enable = true;

  #   # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
  #   intelBusId = "PCI:0:2:0";
  #   # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
  #   nvidiaBusId = "PCI:1:0:0";
  # };

  # Enable OpenGL
  hardware.graphics.enable = true;


  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = [
    "acpi_osi=!"
    ''acpi_osi="Windows 2009"''

    # avoid hard block on susped
    # https://askubuntu.com/a/1057793
    "quiet" "splash"

    "intel_pstate=disable"
  ];

  networking.networkmanager.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp60s0.useDHCP = true;
  networking.interfaces.wlo1.useDHCP = true;
  networking.enableIPv6 = false;

  # Fixing IPv6 bug, see https://github.com/NixOS/nixpkgs/issues/87802#issuecomment-628536317
  networking.networkmanager.dispatcherScripts = [{
    source = pkgs.writeText "upHook" ''
      if [ "$2" != "up" ]; then
      logger "exit: event $2 != up"
      exit
      fi
      echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6
    '';
    type = "basic";
  }];

  # chromecastSupport
  networking.firewall.allowedTCPPorts = [
    8010 # chromecastSupport
  ];
  networking.firewall.allowedUDPPortRanges = [ { from = 32768; to = 60999; } ];
  networking.nameservers = [
    # nordvpn dns servers, from https://support.nordvpn.com/Other/1047409702/What-are-your-DNS-server-addresses.htm
    # "103.86.96.100" "103.86.99.100"
    "1.1.1.1" # "8.8.8.8"
  ];

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  services.avahi.enable = true;

  system.autoUpgrade.enable = true;
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  console.font = "Lat2-Terminus16";
  console.keyMap = "us";

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  environment.pathsToLink = [ "/libexec" ];
  environment.systemPackages = with pkgs; [
    jdk11 jdk17 jdt-language-server
    metals
    blueman
    keychain
    jq killall acpi tlp
    bash wget
    neovim vimPlugins.vim-plug
    # emacs
    curl whois kitty
    harfbuzzFull
    # bash-completion
    eza lsd bat ack silver-searcher fd gnugrep pstree bottom
    openssl wireshark
    udiskie usbutils ipad_charge
    firefox chromium bitwarden vlc # tmux
    keynav xsel xclip xcape htop sqlite
    git gitAndTools.diff-so-fancy
    docker-compose
    lsof pciutils zip unzip unrar bind cacert
    fzf file nnn
    libreoffice-fresh
    nodejs

    python311Packages.pip

    terraform-lsp

    stack visualvm perl shellcheck
    scala_2_13 sbt
    clojure leiningen boot clojure-lsp babashka
    transmission_4 transmission_4-gtk
    networkmanagerapplet arandr
    openvpn gnome.networkmanager-openvpn
    rofi
    gnumake cmake
    entr
    unclutter
    patchelf
    sysstat
    yad
    gettext
    feh
    xdotool
    xorg.xev
    zathura azpainter
    spotify
    gimp
    wine winetricks vulkan-tools
    gnutls libinput-gestures libgpgerror

    parsec-bin

    polybar
    tabbed
    wmctrl
    dunst

    # eww widgets dependencies
    playerctl
    wirelesstools


    # - bspwm
    i3lock-fancy
    sxhkd socat
    xorg.xwininfo

    nvidia-offload

    packages.sxhkd-statusd
    packages.teiler

    packages.icons.pokemon-cursor

    p7zip

    # - tmux-fzf
    bc


    # - kubernets
    # kompose
    # kubectl
    # kubernetes

    kind
    kubectl
  ];

  programs = {
    light.enable = true;
    adb.enable = true;
    steam.enable = true;
  };
  programs.nm-applet.enable = true;

  fonts.packages = with pkgs; [
    powerline-fonts
    font-awesome             # icons
    (nerdfonts.override {    # Nerdfont Icons override
      fonts = [
        "FiraCode"
      ];
    })
  ];

  powerManagement.enable = true;
  powerManagement.powertop.enable = true;

  services.mullvad-vpn = {
    enable = true;
    package = pkgs.mullvad-vpn;
  };

  services.fstrim.enable = true;
  services.tlp.enable = true;

  services.usbmuxd.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  services.blueman.enable = true;
  services.pipewire.alsa.enable = true;

  # Enable the X11 windowing system.
  services.xserver =  {
    enable = true;

    displayManager.lightdm = {
      enable = true;
    };

    displayManager.sessionCommands = ''
      xinput --set-prop "SynPS/2 Synaptics TouchPad" "libinput Accel Speed" 1.0
    '';

    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      extraPackages = with pkgs; [
        dmenu #application launcher most people use
        i3lock #default i3 screen locker
        i3blocks #if you are planning on using i3blocks over i3status
        polybar
     ];
    };

    xkb = {
      layout = "us";
      options = "eurosign:e, caps:ctrl_modifier";
      variant = "intl";
    };

    # "displaylink" to make nix recognize HDMI entry
    # videoDrivers = [ "displaylink" "modesetting" ];
    # videoDrivers = [ "modesetting" ];
    videoDrivers = ["nvidia"];
  };

  services.displayManager.autoLogin.enable = false;

  # Enable touchpad support.
  services.libinput = {
    enable = true;
    touchpad = {
      accelSpeed = "0.9";
      clickMethod = "clickfinger";
      # disableWhileTyping = true;
      # naturalScrolling = true;
      tapping = true;
    };
  };


  hardware.nvidia = {

    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    powerManagement.enable = false;
    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of
    # supported GPUs is at:
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    # Only available from driver 515.43.04+
    # Currently alpha-quality/buggy, so false is currently the recommended setting.
    open = false;

    # Enable the Nvidia settings menu,
    # accessible via `nvidia-settings`.
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  hardware.nvidia.prime = {
    sync.enable = true;
    # Make sure to use the correct Bus ID values for your system!
    intelBusId = "PCI:0:2:0";
    nvidiaBusId = "PCI:1:0:0";
  };

  # services.emacs.enable = true;

  # services.clipmenu.enable = true;

  # testing vpn
  # services.globalprotect.enable = true;

  services.unclutter = {
    enable = true;
    timeout = 10;
  };

  services.logind.lidSwitch = "ignore";
  # services.logind.extraConfig = ''
  #   HandleSuspendKey=hibernate
  #   HandleLidSwitch=hibernate
  # '';

  # services.picom = {
  #   enable = true;
  #   inactiveOpacity = 0.9;
  #   opacityRules = [
  #     "100:class_g = 'Rofi'"
  #     "100:class_g = 'Firefox'"
  #     "100:class_g = 'i3lock'"
  #     "100:class_g = 'i3lock-fancy'"
  #   ];
  # };

  environment.etc = {
    "modprobe.d/blacklist-nouveau.conf".text = ''
      blacklist nouveau
      options nouveau modeset=0
    '';
  };

  # services.udev.extraRules = ''
  #   # https://wiki.archlinux.org/title/Hybrid_graphics#Using_udev_rules
  #   # Remove NVIDIA USB xHCI Host Controller devices, if present
  #   ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c0330", ATTR{power/control}="auto", ATTR{remove}="1"
  #   # Remove NVIDIA USB Type-C UCSI devices, if present
  #   ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c8000", ATTR{power/control}="auto", ATTR{remove}="1"
  #   # Remove NVIDIA Audio devices, if present
  #   ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x040300", ATTR{power/control}="auto", ATTR{remove}="1"
  #   # Remove NVIDIA VGA/3D controller devices
  #   ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x03[0-9]*", ATTR{power/control}="auto", ATTR{remove}="1"
  # '';

  systemd.extraConfig = ''
    DefaultTimeoutStartSec=30s
    DefaultTimeoutStopSec=30s
  '';

  # systemd.packages = [ packages.nordvpn ];
  # systemd.services.nordvpn = {
  #   description = "NordVPN Daemon";
  #   serviceConfig = {
  #     ExecStart="${packages.nordvpn}/bin/nordvpn";
  #     NonBlocking = true;
  #     Restart = "on-failure";
  #     RestartSec = 5;
  #     RuntimeDirectory = "nordvpn";
  #     RuntimeDirectoryMode = "0770";
  #     Group = "wheel"; #"nordvpn";
  #   };
  #   unitConfig = {
  #     StartLimitInterval = "30s";
  #   };
  #   wantedBy = [ "multi-user.target" ];
  #   wants = [ "network-online.target" ];
  #   after = [ "network.target" "network-online.target" ];
  #   requires = [ "nordvpn.socket" ];
  # };

  # systemd.sockets.nordvpn = {
  #   description = "NordVPN Daemon Socket";
  #   wantedBy = [ "sockets.target" ];
  #   socketConfig = {
  #     ListenStream = "/run/nordvpnd.sock";
  #     # ListenStream = "/run/nordvpn/nordvpnd.sock";
  #     NoDelay = true;
  #     SocketMode = "0770";
  #     User = "paulo"; #"nordvpn";
  #     SocketGroup = "wheel"; #"nordvpn";
  #     Service = "nordvpn.service";
  #   };
  #   unitConfig = {
  #     StartLimitInterval = "30s";
  #   };
  # };

  # services.kubernetes = {
  #   roles = [ "master" "node" ];
  #   masterAddress = "localhost";
  #   apiserverAddress = "https://localhost:6443";
  #   easyCerts = true;
  #   apiserver = {
  #     securePort = 6443;
  #     advertiseAddress = "10.0.0.5";
  #   };
  # };
  # systemd.services.etcd.preStart = ''${pkgs.writeShellScript "etcd-wait" ''
  #   while [ ! -f /var/lib/kubernetes/secrets/etcd.pem ]; do sleep 1; done
  # ''}'';
  # services.kubernetes = {
  #   roles = ["master" "node"];
  #   masterAddress = "localhost";
  #   apiserverAddress = "https://localhost:6443";
  #   easyCerts = true;
  #   pki.enable = true;
  #   apiserver = {
  #     securePort = 6443;
  #     advertiseAddress = "10.1.1.2";
  #   };

  #   addons.dashboard.enable = true;

  #   # use coredns
  #   addons.dns.enable = true;

  #   # needed if you use swap
  #   kubelet.extraOpts = "--fail-swap-on=false";
  # };

  virtualisation = {
    docker.enable = true;
    podman.enable = true;
    virtualbox.host.enable = true;
  };

  security.pam.services.gdm.enableGnomeKeyring = true;
  security.sudo.extraRules = [
    {
      commands = [
        {
          command = "${pkgs.light}/bin/light";
          options = [ "NOPASSWD" ];
        }
        {
          command = "${pkgs.tlp}/bin/bluetooth";
          options = [ "NOPASSWD" ];
        }
      ];
      groups = [ "wheel" ];
    }
  ];

  services.gnome.gnome-keyring.enable = true;

  # testing ihp
  # nix.trustedUsers = [ "root" "paulo" ];

  users.groups.nordvpn = {};
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "video" "audio" "networkmanager" "kvm" "render" "sddm" "lightdm" "light" "usbmux" "adbusers" "nordvpn" ];
  };
  # virtualbox
  users.extraGroups.vboxusers.members = [ "${user}" ];

  # nixpkgs.overlays = [ (self: super: {
  #   polybar = super.polybar.override {
  #     pulseSupport = true;
  #     mpdSupport = true;
  #     i3GapsSupport = true;
  #     jsoncpp = super.jsoncpp;
  #     i3 = super.i3;
  #   };
  # })];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "24.05"; # Did you read the comment?

}
