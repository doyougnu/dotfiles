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
  # boot.kernelPackages = pkgs.linuxPackages_6_9;

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
    192.168.1.249 pihole.local
    192.168.1.185 relay.local
    '';

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console.font = "Lat2-Terminus16";
  console.keyMap = "us";

  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # all proprietary
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [

    wget vim binutils man-pages coreutils gnumake iw sshfs
    stdenv pkg-config curl xorg.xmodmap htop aspellDicts.en
    aspell pciutils wirelesstools pavucontrol unzip
    openssl gnutls git libnotify alsaLib xmonad-log
    xorg.xprop xorg.xwininfo xclip xdotool

    ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # set vim to default editor
  environment.variables.VISUAL       = "emacs";
  environment.variables.EDITOR       = "emacs";
  environment.variables.XCURSOR_SIZE = "18";
  environment.variables.EMACS_HOST   = "desktop"; # TODO move to home manager
  # for zsh autocomplete system packages
  environment.pathsToLink = [ "/share/zsh" ];


  users.groups.voltron.gid = 7777;
  users.users.doyougnu = { # don't forget to set a password with passwd
      isNormalUser = true;
      extraGroups = ["networkmanager" "voltron" "wheel" "audio" "pulse" "docker" ];
      uid = 1729;
      shell = pkgs.zsh;
      home = "/home/doyougnu";
    };

  # use flakes and trusted for cachix
  nix = {
     package = pkgs.nixVersions.latest;
     extraOptions = ''
       experimental-features = nix-command flakes
     '';
     settings = {
       trusted-users = [ "root" "doyougnu" ];
       allowed-users =  ["@wheel"];
       trusted-public-keys = [
         "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
         "ghc-nix.cachix.org-1:wI8l3tirheIpjRnr2OZh6YXXNdK2fVQeOI4SVz/X8nA="
       ];
       substituters = [
         "https://cache.iog.io"
         "https://ghc-nix.cachix.org"
       ];
     };
    gc = {
      automatic = true;
      dates     = "weekly";
      options   = "--delete-older-than 14d";
    };
  };

  # device auto mounting
  services.devmon.enable = true;

  # enable sasl
  services.saslauthd.enable = true;

  # logind to never sleep
  powerManagement.enable = false;
  # systemd.targets.sleep.enable = false;
  # systemd.targets.suspend.enable = false;
  # systemd.targets.hybrid-sleep.enable = false;
  systemd.sleep.extraConfig = "HibernateDelaySec=4h";
  services.logind.extraConfig = "IdleAction=ignore";

  ## default is no desktop manager and xmonad
  services.displayManager.defaultSession = "none+xmonad";
  ## fallback to xterm if something should happen
  services.xserver.desktopManager.xterm.enable = true;

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

    # values taken from nvidia-settings
    screenSection = ''
      # Removed Option "metamodes" "DVI-D-0: 1920x1080_120 +0+0, HDMI-0: nvidia-auto-select +1920+0"
    '';

    videoDrivers = [ "nvidia" ];

    displayManager = {
      lightdm.enable = true;
      lightdm.greeters.enso = {
            enable = true;
      # this file cannot bu under /home, it needs to be accessible by the
      # lightdm user so it must be under /usr/share. I've manually copied it
      # there
          extraConfig = ''
              [greeter-theme]
              background-image = "/usr/share/sylvain-sarrailh-bridgehdartstation.jpg"
          '';
        };
      sessionCommands = ''
       ${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
       '';
      setupCommands = ''
       ${pkgs.xorg.xrandr}/bin/xrandr --output DVI-D-0 --primary --mode 1920x1080 --rate 120 --output HDMI-0 --mode 1920x1080 --rotate left --rate 60 --pos 0x0 --left-of DVI-D-0
       '';
    };

  };

  # Drivers 32bit support
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;

  # nice compton settings
  services.compton = {
    enable          = true;
    fade            = true;
    inactiveOpacity = 0.90;
    shadow          = false;
    fadeDelta       = 1;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?

  networking.firewall.allowedTCPPorts = [ 9001 ]; #open default port for cardano-node

  # docker
  virtualisation.docker.enable = true;

  # databases
  services.mysql.enable  = false;
  services.mysql.package = pkgs.mysql;
  services.mysql.group   = "wheel";
}
