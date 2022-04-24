{ lib, ... }:
{
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "slack"
    "spotify"
    "spotify-unwrapped"
    "discord"
    "google-chrome"
    "steam"
  ];
}

