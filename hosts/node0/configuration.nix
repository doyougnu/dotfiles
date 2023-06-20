# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the extlinux boot loader. (NixOS wants to enable GRUB by default)
  boot.loader.grub.enable = false;
  # Enables the generation of /boot/extlinux/extlinux.conf
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "relay"; # Define your hostname.
  networking.networkmanager.enable = false;  # Easiest to use and most distros use this by default.
  networking.wireless.enable = false;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  nixpkgs.config.allowUnfree = true;
  nix = {
    settings.auto-optimise-store = true;
    gc = {
      automatic = true;
      dates     = "weekly";
      options   = "--delete-older-than 30d";
    };
    package = pkgs.nixUnstable;
    extraOptions = ''experimental-features = nix-command flakes '';
    settings = {
      trusted-users = [ "root" "node0" ];
      allowed-users =  ["@wheel"];
      trusted-public-keys = [
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
      substituters = [
        "https://cache.iog.io"
      ];
     };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.node0 = {
    isNormalUser = true;
    isSystemUser = true;
    extraGroups = [ "wheel" "networkmanager" "audio" "pulse" "znc-admin"];
    packages = with pkgs; [
      emacs vim man-pages git
    ];
  };

  # auto login
  services.getty.autologinUser = "node0";

  # cyrusauth module talks to saslauthd, default auth mechanism is PAM
  services.saslauthd.enable = true;
  # znc service config has some hardening options that otherwise block
  # interaction with saslauthd's unix socket
  systemd.services.znc.serviceConfig.RestrictAddressFamilies = [ "AF_UNIX" ];

  # IRC bouncer
  services.znc = {
    enable = true;
    mutable = false;
    useLegacyConfig = false;
    openFirewall = true;

    config = {
      LoadModule = [ "adminlog" "cyrusauth" "saslauthd" ];
      Listener.l =  {
        Port = 48884;
        AllowIRC = true;
        AllowWeb = true;
        SSL = false;
      };
      User.node0 = {
        Admin = true;
      };
    };
  };

  environment.etc = {
    # need to add a PAM service config, cyrusauth identifies itself as "znc"
    # very standard config, copied from others in /etc/pam.d
    # just checks that you have a valid account/password
    "pam.d/znc" = {
      source = pkgs.writeText "znc.pam" ''
      # Account management.
      account required pam_unix.so

      # Authentication management.
      auth sufficient pam_unix.so likeauth try_first_pass
      auth required pam_deny.so

      # Password management.
      password sufficient pam_unix.so nullok sha512

      # Session management.
      session required pam_env.so conffile=/etc/pam/environment readenv=0
      session required pam_unix.so
    '';
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
   environment.systemPackages = with pkgs; [
     vim emacs coreutils wget man-pages git
   ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 8384 22000];
  networking.firewall.allowedUDPPorts = [ 22000 21027];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}

