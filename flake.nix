{
  description = "NixOS configuration and home-manager configurations for mac and debian gnu/linux";
  inputs = {
    unstable.url       = github:nixos/nixpkgs/nixos-unstable;
    nixpkgs.url        = github:nixos/nixpkgs/nixos;
    emacs-overlay.url  = github:nix-community/emacs-overlay;
    nixos-hardware.url = github:nixos/nixos-hardware/master;
    nur.url            = github:nix-community/nur;
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = {emacs-overlay, home-manager, nur, nixos-hardware, nixpkgs, ...}:
    let

      homeManagerConfFor = config: { ... }: {
        nixpkgs.overlays = [ emacs-overlay.overlay nur.overlay ];
        imports = [ config ];
      };
      
      framework-system = home-manager.lib.homeManagerConfiguration {
        configuration = homeManagerConfFor ./hosts/framework/home.nix;
        system = "x86_64-linux";
        homeDirectory = "/home/doyougnu";
        username = "doyougnu";
        stateVersion = "21.11";
      };
    in {
      nixosConfigurations.framework = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          nixos-hardware.nixosModules.framework 
	  ./hosts/framework/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useUserPackages = true;
            home-manager.users.doyougnu = homeManagerConfFor ./hosts/framework/home.nix;
          }
        ];
      };
      framework = framework-system.activationPackage;
      defaultPackage.x86_64-linux = framework-system.activationPackage;
    };
}

