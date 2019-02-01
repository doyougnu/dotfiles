# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

  let 
    unstable = import <unstable> {};
  in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./power-tune.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;
  boot.loader.grub.device = "/dev/sda";
  
  # macbook specific audio stuff
  boot.extraModprobeConfig = ''
  #  options libata.force=noncq
  #  options resume=/dev/sda5
   options snd_hda_intel index=0 model=intel-mac-auto id=PCH
   options snd_hda_intel index=1 model=intel-mac-auto id=HDMI
   options snd_hda_intel model=mbp101
   options hid_apple fnmode=2
  '';
  
   # enable acceleration for 32-bit
   hardware.opengl.driSupport32Bit = true;

  ## Enable pulseaudio and bluetooth
  hardware.bluetooth.enable = true;
  sound.enable = true;
  sound.mediaKeys.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.pulseaudio.zeroconf.discovery.enable = true;

  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.powersave = true;
  networking.hostName = "7thChamber"; # Define your hostname.
  #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };
  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    ohMyZsh.enable = true;
    ohMyZsh.plugins = [ "git" ];
    ohMyZsh.theme = "robbyrussell";
    syntaxHighlighting.enable = true;
  };

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # enable jupyter service for IJulia notebook
  # services.jupyter.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget vim emacs git binutils manpages coreutils gcc gnumake iw sshfs nix
    dzen2 dmenu stdenv pkgconfig zlib spotify slack weechat curl pianobar
    google-chrome xlibs.xmodmap htop neofetch aspellDicts.en openjdk
    z3 aspell texlive.combined.scheme-full ghc stack cabal-install
    mpd pciutils wirelesstools powertop microcodeIntel thermald mbpfan
    evince coq libreoffice gimp z3 rustc zsh-autosuggestions oh-my-zsh
    unzip glibc graphviz alacritty unstable.julia mu offlineimap openssl cacert postfix
    gnutls networkmanagerapplet nix-prefetch-git idris clojure dropbox unstable.leiningen
    cairo xmonad-log playerctl
    ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # set vim to default editor
  environment.variables.EDITOR = "vim";

  # java gui fix for xmonad
  environment.variables._JAVA_AWT_WM_NONREPARENTING="1";

  users.extraUsers.doyougnu = { # don't forget to set a password with passwd
      extraGroups = ["networkmanager" "wheel" "audio" "pulse" "docker" "networkmanager"];
      uid = 1729;
      shell = pkgs.zsh;
      home = "/home/doyougnu";
    };

  # Enable the XServer settings
  services.xserver = {

    # Enable the X11 windowing system.
    enable = true;
    layout = "us";
    libinput.enable = true;
    displayManager = {
      sddm.enable = true;
      sessionCommands = '' ${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap '';
    };

  # set to Mate
  desktopManager = {
    # plasma5.enable = true;
    mate.enable = true;
    # default = "plasma5";
    };
  };

  # enable redshift
  services.redshift = {
    enable = true;
    # provider = "geoclue2";
    latitude = "44";
    longitude = "123";
  };
  # services.redshift.brightness.day = "0.8";
  # services.redshift.brightness.night = "0.4";
  # services.redshift.latitude = "0.0000";
  # services.redshift.longitude = "0.0000";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?
  
  # allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Set emacs to daemon mode
  # services.emacs.enable = true;

  # docker
  virtualisation.docker.enable = true; 

  ## some fonts
  fonts.fonts = with pkgs; 
   [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    mplus-outline-fonts
    dina-font
    proggyfonts
    source-code-pro
    siji
    nerdfonts
    font-awesome_5
    font-awesome_4
    material-icons
    emacs-all-the-icons-fonts
    numix-icon-theme-circle
    hack-font
    ubuntu_font_family
   ];
}
