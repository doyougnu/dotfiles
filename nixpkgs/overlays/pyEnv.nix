# ~/.config/nixpkgs/overlays/myEnv.nix
# install with  nix-env -f "<nixpkgs>" -iA myPy

self: super: {
  myPy = super.buildEnv {
    name = "myPy";
    paths = [
      # A Python 3 interpreter with some packages
      (self.python3.withPackages (
        ps: with ps; [
          pyflakes
          pytest
          python-language-server
          pandas
          numpy
          matplotlib
        ]
      ))

      # Some other packages we'd like as part of this env
      # self.ripgrep
      # self.tmux
    ];
  };
}
