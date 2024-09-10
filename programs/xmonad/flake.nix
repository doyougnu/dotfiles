{
  description = "XMonad configuration";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # haskellPackages = pkgs.haskellPackages.override {
        #   overrides = self: super: {
        #     taffybar = inputs.taffybar;
        #     # dyg-xmonad   = self.callCabal2nix "dyg-xmonad" ./dyg-xmonad {};
        #   };
        # };
      in
      {
        # inherit haskellPackages;
        packages = rec {
          default = dyg-taffybar;
          dyg-taffybar = pkgs.haskellPackages.callCabal2nix "dyg-taffybar" ./taffybar { };
          # dyg-xmonad = haskellPackages.dyg-xmonad;
        };
      }
    );
}
