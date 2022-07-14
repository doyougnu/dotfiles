## architecture taken from: https://github.com/sebastiant/dotfiles
## thanks!
{
  description = "NixOS configuration and home-manager configurations";
  inputs = {
    # nixpkgs.url        = github:nixos/nixpkgs/nixos-unstable;
    nixpkgs.url          = "nixpkgs/nixos-22.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    nixpkgs-local.url    = "/home/doyougnu/programming/nix/nixpkgs";
    emacs-overlay.url  = github:nix-community/emacs-overlay;
    nixos-hardware.url = github:nixos/nixos-hardware/master;
    git-idris2.url     = github:idris-lang/Idris2?rev=5e9a90bd97d3940054dcf2fcaffccff7c72ef5ae;
    nur.url            = github:nix-community/nur;
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = inputs@{emacs-overlay
                   , home-manager
                   , nur
                   , nixos-hardware
                   , nixpkgs
                   , nixpkgs-unstable
                   , nixpkgs-local
                   , git-idris2
                   , ...
                   }:
    let
      system = "x86_64-linux";
      user   = "doyougnu";
      home   = "/home/doyougnu";

      overlay-unstable = final: prev: {
        unstable = nixpkgs-unstable.legacyPackages.${prev.system};
      };

      overlay-local = final: prev: {
        local = nixpkgs-local.legacyPackages.${prev.system};
      };

      idris2-overlay = final: prev: {
        idris2 = git-idris2.packages.${system}.idris2;
      };

      homeManagerConfFor = config: { ... }: {
        nixpkgs.overlays = [ emacs-overlay.overlay nur.overlay overlay-unstable idris2-overlay ];
        imports = [ config ];
      };
      
      framework-system = home-manager.lib.homeManagerConfiguration {
        configuration = homeManagerConfFor ./hosts/framework/home.nix;
        inherit system;
        homeDirectory = home;
        username      = user;
        stateVersion = "21.11";
      };

      desktop-system = home-manager.lib.homeManagerConfiguration {
        configuration = homeManagerConfFor ./hosts/desktop/home.nix;
        inherit system;
        homeDirectory = home;
        username      = user;
        stateVersion = "21.11";
      };
    in 
    {
      nixosConfigurations.framework = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          nixos-hardware.nixosModules.framework
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ overlay-unstable ]; })
          ./hosts/framework/configuration.nix

          home-manager.nixosModules.home-manager {
            home-manager.useUserPackages = true;
            home-manager.users.doyougnu = homeManagerConfFor ./hosts/framework/home.nix;
          }
        ];
      };

      nixosConfigurations.desktop = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ overlay-unstable ]; })
          ./hosts/desktop/configuration.nix

          home-manager.nixosModules.home-manager {
            home-manager.useUserPackages = true;
            home-manager.users.doyougnu = homeManagerConfFor ./hosts/desktop/home.nix;
          }
        ];
      };

      framework = framework-system.activationPackage;
      desktop   = desktop-system.activationPackage;
    };
}

