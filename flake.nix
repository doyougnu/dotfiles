## architecture taken from: https://github.com/sebastiant/dotfiles
## thanks!
{
  description = "NixOS configuration and home-manager configurations";
  inputs = {
    # nixpkgs.url        = github:nixos/nixpkgs/nixos-unstable;
    nixpkgs.url          = "nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    # nixpkgs-local.url    = "/home/doyougnu/programming/nix/nixpkgs";
    keys.url           = "../sync/auth/keys/keys.nix";
    emacs-overlay.url  = github:nix-community/emacs-overlay;
    nixos-hardware.url = github:nixos/nixos-hardware/master;
    git-idris2.url     = github:idris-lang/Idris2?rev=5e9a90bd97d3940054dcf2fcaffccff7c72ef5ae;
    nur.url            = github:nix-community/nur;
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { emacs-overlay
            , home-manager
            , nur
            , nixos-hardware
            , nixpkgs
            , nixpkgs-unstable
            # , nixpkgs-local
            , keys
            , git-idris2
            , ...
            }@attrs :
    let
      system = "x86_64-linux";
      user   = "doyougnu";
      home   = "/home/doyougnu";

      overlay-unstable = final: prev: {
        unstable = nixpkgs-unstable.legacyPackages.${prev.system};
      };

      # overlay-local = final: prev: {
      #   local = nixpkgs-local.legacyPackages.${prev.system};
      # };

      idris2-overlay = final: prev: {
        idris2 = git-idris2.packages.${system}.idris2;
      };

      homeManagerConfFor = config: { ... }: {
        nixpkgs.overlays = [ emacs-overlay.overlay nur.overlay overlay-unstable idris2-overlay ];
        imports = [ config ];
      };

      node0-system = home-manager.lib.homeManagerConfiguration {
        configuration = homeManagerConfFor ./hosts/node0/home.nix;
        inherit system;
        homeDirectory = "/home/node0";
        username      = "node0";
        stateVersion = "21.11";
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
      nixosConfigurations.node0 = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = attrs;
        modules = [
          nixos-hardware.nixosModules.raspberry-pi-4
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ overlay-unstable ]; })
          ./hosts/node0/configuration.nix

          home-manager.nixosModules.home-manager {
            home-manager.useUserPackages = true;
            home-manager.users.node0 = homeManagerConfFor ./hosts/node0/home.nix;
          }
        ];
      };

      nixosConfigurations.framework = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = attrs;
        modules = [
          nixos-hardware.nixosModules.framework
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ overlay-unstable idris2-overlay ]; })
          ./hosts/framework/configuration.nix

          home-manager.nixosModules.home-manager {
            home-manager.useUserPackages = true;
            home-manager.users.doyougnu = homeManagerConfFor ./hosts/framework/home.nix;
          }
        ];
      };

      nixosConfigurations.desktop = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = attrs;
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
      node0     = node0-system.activationPackage;
    };
}

