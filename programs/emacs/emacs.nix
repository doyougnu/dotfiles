/*
This is a nix expression to build Emacs and some Emacs packages I like
from source on any distribution where Nix is installed. This will install
all the dependencies from the nixpkgs repository and build the binary files
without interfering with the host distribution.

To build the project, type the following from the current directory:

$ nix-build emacs.nix

To run the newly compiled executable:

$ ./result/bin/emacs
*/
{ pkgs 
, config
, unstable
}:



let
  # myEmacs = pkgs.emacsUnstable;
  myEmacs = pkgs.emacs.override {
    withGTK3 = true;
    withGTK2 = false;
  };

  emacsWithPackages = (pkgs.emacsPackagesFor myEmacs).emacsWithPackages;

  haskell-env = with unstable.haskell.packages.${config.ghc.version}; [
  ];

in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [


    # magit          # ; Integrate git <C-x g>
    # zerodark-theme # ; Nicolas' theme
  ]) ++ (with epkgs.melpaPackages; [
    # pdf-tools


    # undo-tree      # ; <C-x u> to show the undo tree
    # zoom-frm       # ; increase/decrease font size for all buffers %lt;C-x C-+>
  ]) ++ (with epkgs.elpaPackages; [


    # auctex         # ; LaTeX mode
    # beacon         # ; highlight my cursor when scrolling
    # nameless       # ; hide current package name everywhere in elisp code
  ]) ++ [
    pkgs.notmuch   # From main packages set
    pkgs.cmake
    pkgs.global
    pkgs.ctags
    pkgs.sqlite
    pkgs.sqlint
    pkgs.multimarkdown
    pkgs.shellcheck
    pkgs.nixfmt
    pkgs.gtk3
  ] ++ haskell-env)
