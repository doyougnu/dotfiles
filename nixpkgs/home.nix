{ config, pkgs, ... }:

let sources = import ./nix/sources.nix;
    pkgsOverlaid = import sources.nixpkgs { overlays = [
      (import (builtins.fetchTarball {
        # url = https://github.com/nix-community/emacs-overlay/archive/1e2a3151b27167d2cbe099718ad5bf99de40cd46.tar.gz;
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];};


    unstable = import sources.nixpkgs-unstable {};

    myEmacs = import ./emacs.nix { pkgs = pkgsOverlaid; };

    config  = import ./config.nix;

    haskell-env = with unstable.haskell.packages.${config.ghc.version}; [
      cabal-install
      haskell-language-server
      hlint
      hindent
      apply-refact
      hasktags
      stylish-haskell
    ];

    common-lisp-env = with unstable.lispPackages; [ hunchentoot
                                                    anaphora
                                                    alexandria
                                                    cl-json
                                                    spinneret
                                                    bordeaux-threads
                                                    simple-tasks
                                                  ];
    # haskell-ghc = pkgs.haskell.packages.${config.ghc.version}.ghcWithHoogle
    #   (p: with p; [ mtl
    #                 hspec
    #                 tasty
    #                 tasty-hunit
    #                 sbv
    #   ]);
    services.hoogle = {
      enable = true;
      packages = (hpkgs: with hpkgs; [text mtl containers unordered-containers]);
      haskellPackages = pkgs.haskellPackages;
    };

in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # git setup
  programs.git = {
    enable    = true;
    userEmail = "jeffrey.young@iohk.io";
    userName  = "doyougnu";
    signing.signByDefault = true;
    signing.key = "57403751AE1F59BBC10771F5AF59A1E46422D9C9";
    ignores = [ "TAGS" "GPATH" "GRTAGS" "GTAGS" ".dir-locals.el" "dist-newstyle"
                "*.elc" "*.swp"
              ];
  };

  programs.gpg = {
    enable = true;
    settings = {
      default-key = "0xAF59A1E46422D9C9";
    };
  };
  services.gpg-agent = {
    enable         = true;
    maxCacheTtl    = 7200;
    # pinentryFlavor = "tty";
    extraConfig = ''
    allow-emacs-pinentry
    allow-loopback-pinentry
    '';
  };



  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "doyougnu";
  home.homeDirectory = "/home/doyougnu";


  # services
  services.lorri.enable = true;

  # emacs
  services.emacs.enable = true;
  services.emacs.package = myEmacs;

  programs.fish = {
    enable = true;
    shellAliases = {
      hg = "history | grep";
      e  = "emacsclient -cn";

      nsr = "nix-shell --pure --run";
      nr  = "nix-shell  --run";
      nsc = "nix-shell --pure --command";
      ns  = "nix-shell";
      nsp = "nix-shell -p";
      hms = "nix-shell ~/.config/nixpkgs/shell.nix --run \'home-manager switch\'";

      nivu = "nix-shell -p niv --run 'niv update'";

      hbR   = "nix-shell --pure --run 'hadrian/build clean && ./boot && ./configure && hadrian/build -j --flavour=perf'";
      hbc   = "nix-shell --pure --run 'hadrian/build clean && hadrian/build -j --flavour=perf'";
      hb   = "nix-shell --pure --run 'hadrian/build -j'";
      hbq  = "hb --flavour=quick";
      hbqs = "hbq --skip='//*.mk' --skip='stage1:lib:rts'";
      hbqf = "hbqs --freeze1";
      hbv  = "hb --flavour=validate --build-root=_validate";
      hbvs = "hbv --skip='//*.mk' --skip='stage1:lib:rts'";
      hbvf = "hbvs --freeze1";
      hbt  = "mkdir -p _ticky; [ -e _ticky/hadrian.settings ] || echo 'stage1.*.ghc.hs.opts += -ticky\\nstage1.ghc-bin.ghc.link.o
pts += -ticky' > _ticky/hadrian.settings; hb --flavour=validate --build-root=_ticky";
      hbts = "hbt --skip='//*.mk' --skip='stage1:lib:rts'";
      hbtf = "hbts --freeze1";
    };

    plugins = [{
                 name = "fasd";
                 src = pkgs.fetchFromGitHub
                   {
                     owner  = "oh-my-fish";
                     repo   = "plugin-fasd";
                     rev    = "98c4c729780d8bd0a86031db7d51a97d55025cf5";
                     sha256 = "0m0q0x66b498lxmma9l9qxpzfkms4g7mg26xb6kh2p55vil1547h";
                   };
               }
               {
                 name = "neolambda";
                 src = pkgs.fetchFromGitHub
                   {
                     owner  = "ipatch";
                     repo   = "theme-neolambda";
                     rev    = "9b79e74624de9bbd3405e9feded51b77777b8be7";
                     sha256 = "0id4av6a93h1iczsiqj19r30zjm967ckxxsaa66d830fch65fs4l";
                   };
               }
               {
                 name = "done";
                 src = pkgs.fetchFromGitHub
                   {
                     owner  = "franciscolourenco";
                     repo   = "done";
                     rev    = "7fda8f2c3e79835d5c1e6721fa48fe5ed4ba0858";
                     sha256 = "1snysg52fr1h6n188jhqzny4sfgzcjgpa9r9qvj9smkg7zmplmsy";
                 };
               }];

    shellInit = ''
     ## setup gpg for fish
     ## set -gx GPG_TTY (tty)

     # Add the following to your shell init to set up gpg-agent automatically for every shell
     # if test -f ~/.gnupg/.gpg-agent-info && not pgrep gpg-agent
     #     source ~/.gnupg/.gpg-agent-info
     #     export GPG_AGENT_INFO
     # else
     #     eval (gpg-agent --daemon)
     # end

     function fish_user_key_bindings
       fish_vi_key_bindings
       bind -M insert \ck history-token-search-backward
       bind -M insert \cj history-token-search-forward
       bind -M visual \ck up-or-search
       bind -M visual \cj down-or-search
       bind -M insert fd "if commandline -P; commandline -f cancel; else; set fish_bind_mode default; commandline -f backward-char force-repaint; end"
       bind -M insert \cp up-or-search
       bind -M insert \cn down-or-search
       bind -M visual \cp up-or-search
       bind -M visual \cn down-or-search
       bind -M insert \cl accept-autosuggestion
       bind -M insert -k nul 'accept-autosuggestion execute'
       bind -M visual p  fish_clipboard_paste
     end
      '';

  };

  ## manually load the plugins
  xdg.configFile."fish/conf.d/plugin-neolambda.fish".text = pkgs.lib.mkAfter ''
  for f in $plugin_dir/*.fish
    source $f
  end
  '';

  home.packages = with unstable; [
    cbqn
    chez
    cowsay
    discord
    entr
    firefox
    fasd
    # haskell-ghc
    gerbil
    google-chrome
    guile
    libevent
    libnotify
    moreutils
    myEmacs
    pinentry
    pianobar
    ranger
    ripgrep
    rsync
    sbcl
    sdcv
    shutter
    signal-desktop
    silver-searcher
    spotify
    xclip
    xdg-dbus-proxy
    xdg-desktop-portal
    zip
  ] ++
  [ R
    rEnv
  ]
    ++
    common-lisp-env
    ++
  (with pkgs;
    [ gmp
      numactl
      tdesktop
      valgrind
      thunderbird
  ]);

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
