{ lib, ... }:
{
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "slack"
    "spotify"
    "spotify-unwrapped"
    "discord"
    "parsec-bin"
    "google-chrome"
    "steam"
    "steam-original"
    "steam-runtime"
    "symbola"
  ];
}
