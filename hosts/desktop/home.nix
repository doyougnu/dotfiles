{ config, pkgs, lib, ... }:

let
    myEmacs = pkgs.emacs.override {withGTK3 = true; };
    myTaffybar = pkgs.callPackage ../../programs/taffybar/default.nix {};

    # R-with-packages = pkgs.rWrapper.override { packages = with pkgs.rPackages; [
      # tidyverse cowplot
    # ]; };

    # for mbsync service
    gpg = "/etc/profiles/per-user/doyougnu/bin/gpg2";
    awk = "/run/current-system/sw/bin/awk";

    tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-basic
      dvisvgm dvipng # for preview and export as html
      wrapfig amsmath ulem hyperref capt-of;
    });

    my-fonts = with pkgs; [
      source-code-pro
      siji
      font-awesome_5
      material-icons
      emacs-all-the-icons-fonts
      numix-icon-theme-circle
      # symbola broken on 2024.04
    ];

in {

  imports = [ ../../programs/non-free.nix
            ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  # nixpkgs.config = {
  #   allowUnFree = true;
  #   enableParallelBuildingByDefault = true;
  # };

  # git setup
  programs.git = {
    enable    = true;
    settings.user.email = "jmy6342@gmail.com";
    settings.user.name  = "Jeffrey Young";
    signing.signByDefault = true;
    signing.key = "4C6A178AF6FA3F553ABED0F23AA937E2C385868B";
    ignores = [ "TAGS" "GPATH" "GRTAGS" "GTAGS" ".dir-locals.el" "dist-newstyle"
                "*.elc" "*.swp" ".projectile" ".ignored" "config.sub" ".envrc"
                "config.sub" ".direnv/"
              ];
  };

  # if on new PC make sure you `gpg --import secret.key`
  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enable               = true;
    enableZshIntegration = true;
    defaultCacheTtl = 34560000;
    maxCacheTtl     = 34560000;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
      pinentry-program /home/doyougnu/.nix-profile/bin/pinentry-gtk-2
    '';
  };

  services.unison = {
    enable = true;
    pairs = {
      "sync" = {
        roots = [ "/home/doyougnu/sync" "ssh://node0@relay.local//home/node0/sync"
                ];
        commandOptions = { auto = "true";
                           batch = "true";
                           repeat = "watch+300"; # on change and every five minutes
                           copyonconflict = "true";
                           ui = "text";
                           prefer = "newer";
                         };
      };
    };
  };

  # kitty config
  programs.kitty = {
    enable = true;
      # keybindings = {
      # };
    extraConfig = ''
      cursor none
      copy_on_select yes
      sync_to_monitor no
    '';
    themeFile = "Tomorrow_Night_Blue";
    settings = {
      font_size = "9.0";
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "doyougnu";
  home.homeDirectory = "/home/doyougnu";


  services.dunst = {
    enable = true;
    settings = {
        urgency_low = {
          frame_color = "#268bd2";
          foreground = "#002b36";
          background = "#fdf6e3";
          #timeout = 1;
        };

        urgency_normal = {
          # frame_color = "#b58900";
          background = "#202632";
          foreground = "#ffffff";
          #timeout = 1;
        };

        urgency_critical = {
          # frame_color = "#dc322f";
          background = "#ffffff";
          foreground = "#db0101";
          #timeout = 1;
        };

        global = {
          font = "Iosevka 10";
          width = 500;
          height = 900;
          transparency = 20;
          frame_color = "#4287f5";
          corner_radius = 15;
          # center text
          alignment = "center";
          # full markup parsing
          markup = "full";
          format = ''
          %a
          <b>%s</b>
          %b
          %p'';

          monitor = "1";
          origin = "bottom-center";
          offset = "1x1";

          separator_color = "auto";
          # Width of frame around window
          frame_width = 1;
          # Color of frame around window
        };

    };
  };

  # emacs
  services.emacs.enable  = true;
  services.emacs.package = myEmacs;

  # email
  programs.mbsync.enable = true;        ## sync
  programs.msmtp.enable  = true;        ## sending

  accounts.email = {
    ## blog email
    accounts.blog = {

      address = "jeff@doyougnu.xyz";
      imap.host = "mail.gandi.net";
      mbsync = {
        enable = true;
        create = "maildir";
      };
      msmtp.enable = true;
      primary      = true;
      realName     = "Jeffrey M. Young";
      passwordCommand = "${gpg} -q --for-your-eyes-only --no-tty -d /home/doyougnu/.authinfo.gpg | ${awk} '/machine mail.gandi.net login jeff@doyougnu.xyz password/ {print $6}'";
      smtp = {
        host = "mail.gandi.net";
      };
      userName = "jeff@doyougnu.xyz";
    };

    ## work email
    accounts.iohk = {
      address = "jeffrey.young@iohk.io";
      imap.host = "imap.gmail.com";
      gpg = {
        key = "57403751AE1F59BBC10771F5AF59A1E46422D9C9";
        signByDefault = true;
      };
      mbsync = {
        enable = true;
        create = "maildir";
        patterns = ["*" "![Gmail]*" "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/All Mail"];
        extraConfig = {
          channel = {
            Sync = "All";
          };
        };
      };
      msmtp.enable = true;
      realName     = "Jeffrey M. Young";
      passwordCommand = "${gpg} -q --for-your-eyes-only --no-tty -d /home/doyougnu/.authinfo.gpg | ${awk} '/machine smtp.gmail.com login jeffrey.young@iohk.io password/ {print $6}'";
      smtp = {
        host = "smtp.gmail.com";
      };
      userName = "jeffrey.young@iohk.io";
    };

    ## dumpster email
    accounts.gmail = {
      address = "jmy6342@gmail.com";
      imap.host = "imap.gmail.com";
      mbsync = {
        enable = true;
        create = "maildir";
        patterns = ["*" "![Gmail]*" "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/All Mail"];
        extraConfig = {
          channel = {
            Sync = "All";
          };
        };
      };
      msmtp.enable = true;
      realName     = "Jeffrey M. Young";
      passwordCommand = "${gpg} -q --for-your-eyes-only --no-tty -d /home/doyougnu/.authinfo.gpg | ${awk} '/machine smtp.gmail.com login jmy6342@gmail.com password/ {print $6}'";
      smtp = {
        host = "smtp.gmail.com";
      };
      userName = "jmy6342@gmail.com";
    };
  };

  services.mbsync = {
    enable = true;
    frequency = "*:0/15";
    preExec = "${pkgs.isync}/bin/mbsync -Ha";
    postExec = "${pkgs.unstable.mu}/bin/mu index -m ~/Maildir";
  };

  # write config files
  # polybar
  xdg.configFile = {
    "xmobar" = {
      source = config.lib.file.mkOutOfStoreSymlink /home/doyougnu/dotfiles/programs/xmonad/xmobar;
      recursive = true;
    };
  };

  # xmonad
  home.file.".xmonad/xmonad.hs".source = ../../programs/xmonad/xmonad_desktop.hs;
  home.file.".xmonad/xmonad.hs".onChange = "xmonad --recompile";
  # emacs
  home.file.".emacs.d/early-init.el".source    = ../../programs/emacs/early-init.el;
  home.file.".emacs.d/init.el".source          = ../../programs/emacs/init.el;
  home.file.".emacs.d/personal-org.el".source  = ../../programs/emacs/personal-org.el;
  home.file.".emacs.d/snippets".source         = ../../programs/emacs/doom/snippets;
  home.file.".emacs.d/dyg-keys-mode.el".source = ../../programs/emacs/dyg-keys-mode.el;
  # symlink auth on new hm generation activation
  home.activation = {
      symlinkAuth = lib.hm.dag.entryAfter ["writeBoundary"] ''
                    ln -sf /home/doyougnu/sync/keys/auth/.authinfo.gpg /home/doyougnu/.authinfo.gpg
                    '';
  };
  home.file.".p10k.zsh".source = ../../.p10k.zsh;

  programs.rofi = {
    enable = true;
    theme = "Arc-Dark";
    font = "Hasklug Nerd Font 14";
    extraConfig = { opacity = 80; };
  };

  home.pointerCursor = {
    package = pkgs.numix-cursor-theme;
    name = "Numix-Cursor";
    x11.enable = true;
  };

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    prezto.enable = true;
    prezto.autosuggestions.color = "fg=yellow";
    prezto.prompt.theme = "powerlevel10k";
    prezto.pmodules = [ "git"
                        "environment"
                        "terminal"
                        "history"
                        "editor"
                        "directory"
                        "spectrum"
                        "utility"
                        "completion"
                        "prompt"
                        "fasd"
                        "history-substring-search"
                      ];
    prezto.editor.keymap = "emacs";
    syntaxHighlighting.enable = true;
    initContent = ''
      source ~/.p10k.zsh
      alias hg='history | grep'

      function lookup () { sdcv $1 | less }
      function _lookup () { sdcv $1 }

      function lookup-notify () { notify-send -t 2000 "$(lookup $1)" }

      # Remove any custom keybindings for copy/paste in zsh
      bindkey -r '^O'
      bindkey -r '^E'
      bindkey -r '^V'
      bindkey -r '^D'
      bindkey -r '^S'

      ### --- Basic Emacs movement ---
      bindkey '^A' beginning-of-line          # C-a
      bindkey '^E' end-of-line                # C-e
      bindkey '^B' backward-char              # C-b
      bindkey '^F' forward-char               # C-f
      bindkey '^P' up-line-or-history         # C-p
      bindkey '^N' down-line-or-history       # C-n

      ### --- Word movement like Emacs ---
      bindkey '^[b' backward-word             # M-b
      bindkey '^[f' forward-word              # M-f

      ### --- Kill / yank (cut / paste) ---
      bindkey '^K' kill-line                  # C-k
      bindkey '^Y' yank                       # C-y
      bindkey '^[d' kill-word                 # M-d

      ### --- Transpose / delete / insert ---
      bindkey '^T' transpose-chars            # C-t
      bindkey '^D' delete-char                # C-d

      ### --- Eshell-style incremental search ---
      # bindkey '^R' history-incremental-search-backward  # C-r
      bindkey '^S' autosuggest-accept       # C-s accept autosuggestion
      # bindkey -e "^[s" autosuggest-accept

      # bindkey "^t" history-beginning-search-backward
      # bindkey "^n" history-beginning-search-forward
      # bindkey "^[t" history-search-backward
      # bindkey "^[n" history-search-forward
      # bindkey -e "^t" up-line-or-history
      # bindkey -e "^n" down-line-or-history
      # bindkey -e "^w" forward-word
      # bindkey "^w" forward-word
      # bindkey -e "^b" backward-word
      # bindkey "^b" backward-word
      # bindkey -e "^d" kill-word
      # bindkey "^d" kill-word
      # bindkey -e "^H" backward-kill-word
      # bindkey "^H" backward-kill-word
      # bindkey "^[k" kill-whole-line
      # bindkey "^[i" expand-or-complete-with-indicator

      # Set the autosuggestion highlight style to a lava-like color
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#ff4500'
    '';
    shellAliases = {
      rgf = "rg --files | rg";
      e  = "emacsclient -nw --create-frame";

      gs = "git status";
      gp = "git push";
      gsu = "git submodule update --recursive";
      nix = "noglob nix";
      nixos-rebuild = "noglob nixos-rebuild";

      # fasd aliases
      a  = "fasd -a";       # any
      s  = "fasd -si";      # show / search / select
      d  = "fasd -d";       # directory
      f  = "fasd -f";       # file
      sd = "fasd -sid";     # interactive directory selection
      sf = "fasd -sif";     # interactive file selection
      z  = "fasd_cd -d";    # cd, same functionality as j in autojump
      zz = "fasd_cd -d -i"; # cd with interactive selection
      l  = "ll";
      c  = "cd";

    };
    sessionVariables = {
      GPG_TTY = "$(tty)";
    };
  };

  home.packages = with pkgs; [
    alsa-utils
    chez
    cachix
    clang-tools # need to be system-wide for emacs
    discord
    entr
    evince
    element-desktop
    myEmacs
    fasd
    feh
    gcc            # for org-roam
    google-chrome
    gforth # aoc2023
    libevent
    killall         # for polybar launch script
    moreutils
    multimarkdown
    pdfpc           # pdf presentaitons from the shell
    pulseaudio-ctl  # music control in xmonad
    pinentry-gtk2
    python310
    python310Packages.pygments
    yazi
    ripgrep
    # rnix-lsp
    rsync
    # sbcl
    # sdcv             # for polybar
    spotify
    slack
    tex
    xfce.thunar
    unison
    xclip
    # xorg.xwininfo    # for emacs everywhere
    # xdotool          # for emacs everywhere
    xdg-dbus-proxy
    xdg-desktop-portal
    xmonad-log
    xmobar           # bar for xmonad
    w3m              # text browser for emacs-w3m
    sqlite
    wordnet
    zip
  ] ++
    my-fonts
    ++
  (with unstable;
    [ gmp
      numactl
      flameshot
      telegram-desktop
      pianobar
      valgrind
      perf
      firefox
  ]);

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";
}
