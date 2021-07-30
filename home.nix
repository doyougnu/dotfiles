{ config, pkgs, ... }:

let unstable = import <unstable> { overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];};
    myEmacs = import ./emacs.nix { pkgs = unstable; };

in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "doyougnu";
  home.homeDirectory = "/home/doyougnu";

  # services
  services.lorri.enable = true;

 programs.zsh = {
    enable = true;
    autocd = true;
    dotDir = ".config/zsh";
    enableAutosuggestions = true;
    enableCompletion      = true;
    shellAliases = {
      hg = "history | grep";

      nsr = "nix-shell --pure --run";
      nsc = "nix-shell --pure --command";
      ns  = "nix-shell";
      hm = "home-manager";

      hb   = "hadrian/build -j";
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

    initExtraBeforeCompInit = "source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
    initExtra = ''
      bindkey -v "^k" history-substring-search-up
      bindkey -v "^j" history-substring-search-down
      bindkey -v "^p" up-line-or-history
      bindkey -v "^n" down-line-or-history
      bindkey -v "fd" vi-cmd-mode
      bindkey -v "^l" autosuggest-accept
      bindkey -v "^ " autosuggest-execute

      [[ ! -f ~/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh
      source ~/.config/zsh/plugins/functions ## there's probably a better way but this works
      '';

    plugins = [
      {
        name = "powerlevel10k";
        src  = unstable.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
    ];
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "history-substring-search" "z" "colored-man-pages"
                ];
    };

  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  home.packages = with unstable; [
    chez
    chromium
    cowsay
    discord
    dunst
    entr
    firefox
    haskell-language-server
    ghc
    gerbil
    guile
    libevent
    libnotify
    moreutils
    myEmacs
    pinentry
    ranger
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
    # xfce4-notifyd
    zip
  ] ++
  (with pkgs;
    [ tdesktop
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
