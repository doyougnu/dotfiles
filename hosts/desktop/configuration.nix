# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  baseconfig    = { allowUnfree = true; allowBroken = true; };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  ## Enable pulseaudio and bluetooth
  sound.enable = true;
  sound.mediaKeys.enable = true;
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;
  hardware.bluetooth.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.pulseaudio.zeroconf.discovery.enable = true;

  networking.networkmanager.enable = true;
  networking.hostName = "7thChamber"; # Define your hostname.
  networking.extraHosts =
    ''
    192.168.0.152 pihole.local
    192.168.0.103 hypervisor.local
    '';

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console.font = "Lat2-Terminus16";
  console.keyMap = "us";

  programs.fish.enable = true;
#   programs.zsh = {
#     enable = true;
#     autosuggestions.enable = true;
#     ohMyZsh.enable = true;
#     ohMyZsh.plugins = [ "git" "history-substring-search" "z" "colored-man-pages" ];
#    syntaxHighlighting.enable = true;
#     promptInit = "source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
#   };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # all proprietary
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [

    wget vim binutils man-pages coreutils gnumake iw sshfs
    stdenv pkgconfig curl xorg.xmodmap htop aspellDicts.en
    aspell pciutils wirelesstools pavucontrol unzip
    openssl gnutls git libnotify emacs alsaLib xmonad-log
    dmenu kitty xorg.xprop xorg.xwininfo xclip xdotool
    autorandr

    ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.hoogle = {
    enable = true;
    haskellPackages = pkgs.haskellPackages;
  };

  services.syncthing = {
    enable    = true;
    package   = pkgs.unstable.syncthing;
    user      = "doyougnu";
    dataDir   = "/home/doyougnu/sync";
    configDir = "/home/doyougnu/.config/syncthing";
    devices = {
      "voltron"    = { id = "QXVXZ3O-M3WUXGW-HVQOIJM-XHIL2QG-AIG5AJW-PYNZF6W-4T7YCUW-N7HFHQC"; };
      "7thChamber-framework" = { id = "QFU3KKN-B6Q5UM5-DL7A5YF-GCBP7OJ-FUARMRP-IXVRNQE-J3DXJSI-N3B4CQR"; };
      "7thChamber-desktop"   = { id = "I3UKPJJ-TU3N6OF-4F2REJ5-5QGMHDW-SIKIIDL-ZZU3CVC-BRQJOME-C5Z73AG"; };
    };
    folders = {
      "org" = {
        id      = "a5she-s5zwp";
        path    = "/home/doyougnu/sync/org";
        devices = [ "voltron" "7thChamber-framework" "7thChamber-desktop" ];
      };
      "Books" = {
        id      = "hq3xr-sqxfw";
        path    = "/home/doyougnu/sync/Books";
        devices = [ "voltron" "7thChamber-framework" "7thChamber-desktop" ];
      };
      "dnd" = {
        id      = "ntwfo-bgvsq";
        path    = "/home/doyougnu/sync/dnd";
        devices = [ "voltron" "7thChamber-framework" "7thChamber-desktop" ];
      };
      "other" = {
        id      = "vshib-2mbq2";
        path    = "/home/doyougnu/sync/other";
        devices = [ "voltron" "7thChamber-framework" "7thChamber-desktop" ];
      };
      "deft" = {
        id      = "wzppz-wf5vh";
        path    = "/home/doyougnu/sync/deft";
        devices = [ "voltron" "7thChamber-framework" "7thChamber-desktop" ];
      };
      "wallpapers" = {
        id      = "ydge2-qncvp";
        path    = "/home/doyougnu/sync/wallpapers";
        devices = [ "voltron" "7thChamber-framework" "7thChamber-desktop" ];
      };
      "keys" = {
        id      = "yglju-g4hng";
        path    = "/home/doyougnu/sync/keys";
        devices = [ "voltron" "7thChamber-framework" "7thChamber-desktop" ];
      };
      "roam" = {
        id      = "oezuf-gad5a";
        path    = "/home/doyougnu/sync/roam";
        devices = [ "voltron" "7thChamber-framework" "7thChamber-desktop" ];
      };
    };
    overrideFolders = true;
    overrideDevices = true;
  };


  # set vim to default editor
  environment.variables.VISUAL       = "emacs";
  environment.variables.EDITOR       = "emacs";
  environment.variables.XCURSOR_SIZE = "18";
  environment.variables.EMACS_HOST   = "desktop"; # TODO move to home manager

  users.groups.voltron.gid = 7777;
  users.extraUsers.doyougnu = { # don't forget to set a password with passwd
      isNormalUser = true;
      extraGroups = ["networkmanager" "voltron" "wheel" "audio" "pulse" "docker" ];
      uid = 1729;
      shell = pkgs.fish;
      home = "/home/doyougnu";
    };

  # use flakes and trusted for cachix
  nix = {
     package = pkgs.nixUnstable;
     extraOptions = ''
       experimental-features = nix-command flakes
     '';
     settings = {
       trusted-users = [ "root" "doyougnu" ];
       allowed-users =  ["@wheel"];
       trusted-public-keys = [
         "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
       ];
       substituters = [
         "https://cache.iog.io"
       ];
     };
  };

  # device auto mounting
  services.devmon.enable = true;

  # logind to never sleep
  powerManagement.enable = false;
  # systemd.targets.sleep.enable = false;
  # systemd.targets.suspend.enable = false;
  # systemd.targets.hybrid-sleep.enable = false;
  systemd.sleep.extraConfig = "HibernateDelaySec=4h";
  services.logind.extraConfig = "IdleAction=ignore";

  # Enable the XServer settings
  services.xserver.enable = true;
  services.xserver = {

    # Enable the X11 windowing system.
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
         haskellPackages.dbus
         haskellPackages.xmonad-spotify
      ];
    };

    layout = "dyg-dvorak,";
    xkbVariant = "dvorak";
    videoDrivers = [ "nvidia" ];

    extraLayouts.dyg-dvorak = {
      description = "My custom layout";
      languages   = [ "eng" ];
      symbolsFile = ../../programs/symbols/dyg-dvorak;
    };

    displayManager = {
      lightdm.enable = true;
      autoLogin = {
        enable = false;
        user = "doyougnu";
        };
      sessionCommands = ''
       ${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
       '';
    };

  };

  ## default is no desktop manager and xmonad
  services.xserver.displayManager.defaultSession = "none+xmonad";

  # Drivers 32bit support
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;

  # nice compton settings
  services.compton = {
    enable          = true;
    fade            = true;
    inactiveOpacity = 0.80;
    shadow          = false;
    fadeDelta       = 1;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?



  # docker
  virtualisation.docker.enable = true;

  # databases
  services.mysql.enable  = false;
  services.mysql.package = pkgs.mysql;
  services.mysql.group   = "wheel";

  ## some fonts
  fonts.fonts = with pkgs; [
    source-code-pro
    siji
    nerdfonts             # nerdfonts broken on stable for 21.11
    font-awesome_5
    font-awesome_4
    material-icons
    emacs-all-the-icons-fonts
    numix-icon-theme-circle
    symbola
   ];
}
