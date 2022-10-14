# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, nixpkgs-local, ... }:

  let
  baseconfig = { allowUnfree = true; };
  in
  {
  imports =
    [ ./hardware-configuration.nix
    ];


  # use the latest linux kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;
  boot.kernel.sysctl = {
    "dev.i915.perf_stream_paranoid" = 0;
  };
  services.logind.lidSwitch = "suspend-then-hibernate";
  services.logind.extraConfig = "IdleActionSec=300min";

  # power
  services.tlp.enable = true;
  powerManagement.enable = true;
  powerManagement.powertop.enable = true;

  # sound
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # framework firmware update
  services.fwupd.enable = true;

  # enable acceleration for 32-bit
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      libva
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
      intel-compute-runtime
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [
      libva
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
  };
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.powersave = true;
  networking.hostName = "7thChamber"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.extraHosts =
    ''
    192.168.0.152 pihole.local
    192.168.0.102 hypervisor.local
    192.168.0.104 elminster.local
    '';

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  programs.fish = {
    enable = true;
  };

  # use flakes and trusted for cachix
  nix = {
     package = pkgs.nixUnstable;
     extraOptions = ''
       experimental-features = nix-command flakes
     '';
     allowedUsers = ["@wheel"];
     trustedUsers = [ "root" "doyougnu" ];
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # automatically garbage collect packages
  nix.gc = {
  automatic = true;
  dates = "monthly";
  options = "--delete-older-than 15d";
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  # ghc needed for xmonad, get some error with alibsound2 without it
  environment.systemPackages = with pkgs; [
    wget vim binutils man-pages coreutils gnumake iw sshfs
    stdenv pkgconfig curl xorg.xmodmap htop aspellDicts.en
    aspell pciutils wirelesstools pavucontrol unzip
    openssl gnutls git libnotify emacs alsaLib xmonad-log
    dmenu kitty
  ];

  # List services that you want to enable:
  services.syncthing = {
    enable    = true;
    package   = pkgs.unstable.syncthing;
    user      = "doyougnu";
    dataDir   = "/home/doyougnu/sync";
    configDir = "/home/doyougnu/.config/syncthing";
    devices = {
      "voltron" = { id = "QXVXZ3O-M3WUXGW-HVQOIJM-XHIL2QG-AIG5AJW-PYNZF6W-4T7YCUW-N7HFHQC"; };
    };
    folders = {
      "org" = {
        id      = "a5she-s5zwp";
        path    = "/home/doyougnu/sync/org";
        devices = [ "voltron" ];
      };
      "Books" = {
        id      = "hq3xr-sqxfw";
        path    = "/home/doyougnu/sync/Books";
        devices = [ "voltron" ];
      };
      "dnd" = {
        id      = "ntwfo-bgvsq";
        path    = "/home/doyougnu/sync/dnd";
        devices = [ "voltron" ];
      };
      "other" = {
        id      = "vshib-2mbq2";
        path    = "/home/doyougnu/sync/other";
        devices = [ "voltron" ];
      };
      "deft" = {
        id      = "wzppz-wf5vh";
        path    = "/home/doyougnu/sync/deft";
        devices = [ "voltron" ];
      };
      "wallpapers" = {
        id      = "ydge2-qncvp";
        path    = "/home/doyougnu/sync/wallpapers";
        devices = [ "voltron" ];
      };
      "keys" = {
        id      = "yglju-g4hng";
        path    = "/home/doyougnu/sync/keys";
        devices = [ "voltron" ];
      };

    };
    overrideFolders = true;
    overrideDevices = true;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPortRanges = [ { from = 32768; to = 60999; } ];
  networking.firewall.allowedTCPPorts = [ 8384 22000];
  networking.firewall.allowedUDPPorts = [ 22000 21027];

  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # set emacs to default editor
  environment.variables.EDITOR                = "emacs";
  environment.variables.WINIT_HIDPI_FACTOR    = "1";
  environment.variables.GPG_TTY               = "$(tty)";
  environment.variables.VDPAU_DRIVER          = "va_gl";
  environment.variables.EMACS_HOST            = "framework"; # TODO move to home manager

  # enable blueman service for bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
#   hardware.bluetooth.settings = { General = { ControllerMode = "dual";
#                                               Enable = "Source,Sink,Media,Socket";
#                                             }; };
#   hardware.bluetooth.disabledPlugins = [ "avrcp" ];

  # java gui fix for xmonad
  environment.variables._JAVA_AWT_WM_NONREPARENTING="1";

  environment.variables.GTAGSLABEL="ctags";

  users.extraUsers.doyougnu = { # don't forget to set a password with passwd
      isNormalUser = true;
      extraGroups = ["syncthing" "networkmanager" "wheel" "audio" "pulse" "docker" "video"];
      uid = 1729;
      shell = pkgs.fish;
      home = "/home/doyougnu";
    };

  # Enable the XServer settings
  services.xserver = {

    # Enable the X11 windowing system.
    enable = true;
    layout = "us";
    dpi    = 120;
    libinput.enable = true;

    extraLayouts.dyg-dvorak = {
      description = "My custom layout";
      languages   = [ "eng" ];
      symbolsFile = ../../programs/symbols/dyg-dvorak;
    };

     windowManager.xmonad = {
       enable = true;
       enableContribAndExtras = true;
       extraPackages = haskellPackages: [
         haskellPackages.dbus
         pkgs.xmonad-log
         haskellPackages.xmonad-spotify
       ];
     };
     displayManager = {
      defaultSession = "none+xmonad";
      lightdm.enable = true;
      autoLogin.user   = "doyougnu";
      sessionCommands = ''
         ${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
         '';
    };
  };
  environment.variables.XMONAD_CONFIG_DIR = "/home/doyougnu/.xmonad";
  environment.variables.XMONAD_CACHE_DIR  = "/home/doyougnu/.xmonad";
  environment.variables.XMONAD_DATA_DIR   = "/home/doyougnu/.xmonad";

  # nice compton settings
  services.compton = {
  enable          = true;
  fade            = true;
  inactiveOpacity = 0.80;
  shadow          = false;
  fadeDelta       = 1;
  };

  programs.light.enable = true;

  # enable redshift
  services.redshift.enable = true;
  location.provider  = "manual";
  location.latitude  = 42.8;
  location.longitude = 78.8;

  # set autoupgrade channel to most recent
  system.autoUpgrade.channel = https://nixos.org/channels/nixos-19.09;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

  # docker
  virtualisation.docker.enable = false;

  # postgres
  # services.mysql.enable  = true;
  # services.mysql.package = pkgs.mysql;
  # services.mysql.group   = "wheel";
  # services.postgresql.package = pkgs.postgresql_10;

  ## some fonts
  fonts.fonts = with pkgs;
   [
    source-code-pro
    siji
    nerdfonts
    font-awesome_5
    font-awesome_4
    material-icons
    emacs-all-the-icons-fonts
    numix-icon-theme-circle
    symbola
   ];

  ## mopidy config
  ## services.mopidy = {
  ## enable = true;
  ## extensionPackages = [ pkgs.mopidy-spotify pkgs.mopidy-soundcloud ];
  ## configuration = ''
  ##   [audio]
  ##   output = pulsesink server=127.0.0.1

  ##   [mpd]
  ##   hostname = 127.0.0.1

  ##   [local]
  ##   enabled = true
  ##   media_dir = /home/doyougnu/Music

  ##   [http]
  ##   enabled = true
  ##   hostname = 127.0.0.1

  ##   [soundcloud]
  ##   auth_token = 3-35204-563267142-E24Rg5LxmAhpXtZ

  ##   [spotify]
  ##   username = --
  ##   password = --
  ##   client_id = 430441f3-9ccd-4fa7-b81f-d5e9d26fef74
  ##   client_secret = fZveMNoy-_no_k-C0jGAYLFVLQAWbpshDGSBMSGv9t8=
  ## '';
  ## };


}
