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
      ../../programs/kmonad/kmonad.nix
    ];


  # use the latest linux kernel
  # boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmp.cleanOnBoot = true;
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
  networking.extraHosts =
    ''
    192.168.1.249 pihole.local
    192.168.1.185 relay.local
    '';

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
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
    wget vim binutils man-pages coreutils gnumake iw sshfs stdenv pkg-config curl
    htop aspellDicts.en aspell pciutils wirelesstools pavucontrol unzip openssl
    gnutls git libnotify alsaLib xmonad-log xorg.xprop xorg.xwininfo
    xclip xdotool
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # fingerprint sensor
  security.polkit.enable  = true;
  services.fprintd.enable = true;

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
  environment.pathsToLink = [ "/share/zsh" ];

  # enable blueman service for bluetooth
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  services.blueman.enable = true;
  hardware.bluetooth.settings = {
    General =
      { Experimental = true;
        Enable = "Source,Sink,Media,Socket";
      };
                                };
#   hardware.bluetooth.settings = { General = { ControllerMode = "dual";
#                                             }; };
#   hardware.bluetooth.disabledPlugins = [ "avrcp" ];

  # java gui fix for xmonad
  environment.variables._JAVA_AWT_WM_NONREPARENTING="1";

  environment.variables.GTAGSLABEL="ctags";

  users.users.doyougnu = { # don't forget to set a password with passwd
      isNormalUser = true;
      extraGroups = [ "networkmanager" "wheel" "audio" "pulse"
                      "docker" "video" "input" "uinput"
                    ];
      uid = 1729;
      shell = pkgs.zsh;
      home = "/home/doyougnu";
    };

  # Enable the XServer settings
  services.libinput.enable = true;
  services.displayManager.defaultSession = "none+xmonad";
  services.displayManager.autoLogin.user = "doyougnu";
  services.xserver = {

    # Enable the X11 windowing system.
    enable = true;
    # layout = "dyg-dvorak";
    xkb.options = "compose:ralt";
    dpi    = 120;

    # extraLayouts.dyg-dvorak = {
    #   description = "My custom layout";
    #   languages   = [ "eng" ];
    #   symbolsFile = ../../programs/symbols/dyg-dvorak;
    # };

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
      lightdm.enable = true;
      sessionCommands = ''
         ${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
         '';
    };
  };
  environment.variables.XMONAD_CONFIG_DIR = "/home/doyougnu/.xmonad";
  environment.variables.XMONAD_CACHE_DIR  = "/home/doyougnu/.xmonad";
  environment.variables.XMONAD_DATA_DIR   = "/home/doyougnu/.xmonad";

  # kmonad layers and keyboard
   services.kmonad = {
     enable = true;
     keyboards = {
       "framework_keyboard" = {
         device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
         config = ''
          (defcfg
            ;; For Linux
            input  (device-file "/dev/input/by-path/platform-i8042-serio-0-event-kbd")
            output (uinput-sink "My KMonad output")

            ;; This option tells KMonad to let non-configured keys act normal
            fallthrough true)

          (defsrc
            grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
            tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
            caps a    s    d    f    g    h    j    k    l    ;    '    ret
            lsft z    x    c    v    b    n    m    ,    .    /    rsft
            lctl lmet lalt           spc            ralt rctl
          )

          (defalias
              ;; home row mods base
              home_a (tap-hold-next-release 160 a lmet)

              home_o (tap-hold-next-release 160 o lalt)
              home_e (tap-hold-next-release 160 e lctl)

              home_, (tap-hold-next-release 160 , lalt)
              home_. (tap-hold-next-release 160 . lctl)
              home_; (tap-hold-next-release 160 u lsft)

              home_z (tap-hold-next-release 160 h rsft)

              home_t (tap-hold-next-release 160 t rctl)
              home_n (tap-hold-next-release 160 n lalt)

              home_c (tap-hold-next-release 160 c rctl)
              home_r (tap-hold-next-release 160 r lalt)
              home_s (tap-hold-next-release 160 s rmet)

             ;; symbols
             syms (layer-toggle sym)

             ;; num
             nums (layer-toggle num)

             ;; special handling for underscore
             _ (around lshft -)
          )

          (deflayer base
            XX  XX    XX    XX    XX    XX    XX    XX    XX    XX    XX    XX    XX    bspc
            tab  '    @home_,    @home_.    p    y   f    g    @home_c    @home_r    l    XX   XX    XX
            @syms @home_a   o    e   u    i    d   h   t   n   @home_s @nums ret
            lshft @home_;    q    j    k    x    b    m    w    v    @home_z    rsft
            lctl lmet esc    spc            ret rctl
          )

          (deflayer sym
            XX   XX   XX   XX   XX    XX   XX   XX   XX   XX   XX   XX   XX   bspc
            tab  "    <    >    ^     [    ]    &    !    /    XX   XX   XX   XX
            caps XX   `    :    ?    \(   \)    =    $    |    @_    -   ret
            lsft XX  XX    ~    @     {    }    #    %    \    XX rsft
            lctl lmet lalt           spc            ret rctl
          )

          (deflayer num
            XX  XX    XX    XX    XX    XX    XX    XX    XX    XX    XX    XX    XX    bspc
            tab  *    <    >    +   XX    XX    XX    XX    XX    XX    XX    XX    XX
            caps 0     1    2   3    4    XX    XX    XX    XX    XX    XX ret
            lsft 5    6   7    8    9     XX    XX    XX    XX    XX       rsft
            lctl lmet lalt           spc            ret rctl
          )
         '';
         };
       };
     package = pkgs.kmonad;
  };

  # enable sasl
  services.saslauthd.enable = true;

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
  virtualisation.docker.enable = true;

  # postgres
  # services.mysql.enable  = true;
  # services.mysql.package = pkgs.mysql;
  # services.mysql.group   = "wheel";
  # services.postgresql.package = pkgs.postgresql_10;

  ## some fonts
  # fonts.packages = with pkgs;
  #  [
  #  ];

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
