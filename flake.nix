## architecture taken from: https://github.com/sebastiant/dotfiles
## thanks!
{
  description = "NixOS configuration and home-manager configurations";
  inputs = {
    # nixpkgs.url        = github:nixos/nixpkgs/nixos-unstable;
    nixpkgs.url          = "nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "nixpkgs/nixos-21.11";
    emacs-overlay.url  = github:nix-community/emacs-overlay;
    nixos-hardware.url = github:nixos/nixos-hardware/master;
    nur.url            = github:nix-community/nur;
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = inputs@{emacs-overlay, home-manager, nur, nixos-hardware, nixpkgs, nixpkgs-stable, ...}:
    let
      system = "x86_64-linux";
      user   = "doyougnu";
      home   = "/home/doyougnu";
      overlay-stable = final: prev: {
        stable = nixpkgs-stable.legacyPackages.${prev.system};
      };
      homeManagerConfFor = config: { ... }: {
        nixpkgs.overlays = [ emacs-overlay.overlay nur.overlay overlay-stable ];
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
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ overlay-stable ]; })
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
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ overlay-stable ]; })
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

