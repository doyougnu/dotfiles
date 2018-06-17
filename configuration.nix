# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "Voltron"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  # environment.systemPackages = with pkgs; [
  #   wget vim
  # ];

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
  services.printing.enable = false;

  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Disable touchpad support.
  # services.xserver.libinput.enable = false;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.displayManager.lightdm.enable = true;
  # services.xserver.desktopManager.xfce.enable = true;
  services.xserver = {

    # Enable the X11 windowing system.
    enable = true;
    layout = "us";

    # Disable touchpad support.
    libinput.enable = false;

    # AMD Settings
    # use open source driver
    videoDrivers = [ "ati" ];

    # Use Xmonad as the window manager
    # windowManager.xmonad = {
    # enable = true;
    # enableContribAndExtras = true;
    # };

    # Display and DE
    # displayManager.sddm.enable = true;
    displayManager = {
      sddm.enable = true;
      sessionCommands = '' ${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap '';
    };
    desktopManager.plasma5.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.doyougnu = {
    isNormalUser = true;
    shell = pkgs.fish;
    home = "/home/doyougnu";
    extraGroups = [ "wheel" "audio" "pulse" "sound" "docker"];
    uid = 1729;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

  ############## Begin My Config ######################
  ## Allow unfree
  nixpkgs.config.allowUnfree = true;

  ## Absolutely Essential Packages
  environment.systemPackages = with pkgs; [ emacs
                                            vim
                                            wget
                                            git
                                            binutils
                                            manpages
                                            coreutils
                                            rxvt_unicode
                                            gcc
                                            gnumake
                                            iw
                                            sshfs
                                            nix
                                            dzen2
                                            dmenu
                                            stdenv
                                            pkgconfig
                                            zlib
                                            # spotify
                                            slack
                                            weechat
                                            curl
                                            pianobar
                                            drive
                                            google-chrome
                                            alsaLib
                                            xlibs.xmodmap
                                            htop
                                            neofetch
                                            aspellDicts.en
                                            leiningen
                                            openjdk
                                            z3
                                            aspell
                                            texlive.combined.scheme-full
                                            cairo
                                            # haskell dependencies
                                            ghc
                                            stack
                                            cabal-install
                                          ];

  # enable acceleration for 32-bit
  hardware.opengl.driSupport32Bit = true;

  ## Enable pulseaudio
  sound.enable = true;
  sound.mediaKeys.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.pulseaudio.zeroconf.discovery.enable = true;

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
   ];

  # docker
  virtualisation.docker.enable = true;

  # use emacs daemon
  services.emacs.enable = true;
}
