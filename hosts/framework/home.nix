{ pkgs, config, lib, ... }:

let
    # myEmacs = import ../../programs/emacs/emacs.nix { pkgs = pkgs; config = config; unstable = pkgs; };
    myEmacs = pkgs.emacs29.override {withGTK3 = true; };

    R-with-packages = pkgs.rWrapper.override { packages = with pkgs.rPackages; [
      tidyverse cowplot
    ]; };

    # for mbsync service
    gpg = "/etc/profiles/per-user/doyougnu/bin/gpg2";
    awk = "/run/current-system/sw/bin/awk";

    my-fonts = with pkgs; [
      source-code-pro
      # siji
      nerdfonts
      font-awesome_5
      # font-awesome_4
      # material-icons
      emacs-all-the-icons-fonts
      # numix-icon-theme-circle
      symbola
    ];

in {

  imports = [ ../../programs/non-free.nix
            ];

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
                "*.elc" "*.swp" ".projectile" ".ignored" "config.sub" ".envrc"
                "config.sub"
              ];
  };

  # if on new PC make sure you `gpg --import secret.key`
  programs.gpg = {
    enable = true;
    settings = {
      default-key = "0xAF59A1E46422D9C9";
    };
  };

  # kitty config
  programs.kitty = {
    enable = true;
      keybindings = {
        "ctrl+c" = "copy_to_clipboard";
        "ctrl+v" = "paste_from_clipboard";
      };
    extraConfig = ''
      copy_on_select yes
    '';
    theme = "Tomorrow Night Blue";
    settings = {
      font_size = "14.0";
    };
  };


  services.gpg-agent = {
    enable               = true;
    enableZshIntegration = true;
    defaultCacheTtl = 34560000;
    maxCacheTtl     = 34560000;
    extraConfig = ''
    allow-emacs-pinentry
    allow-loopback-pinentry
    pinentry-program /home/doyougnu/.nix-profile/bin/pinentry
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

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };


  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "doyougnu";
  home.homeDirectory = "/home/doyougnu";


  fonts.fontconfig.enable = true;

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
          font = "Iosevka 12";
          width = 500;
          height = 900;
          transparency = 20;
          # frame_color = "#eceff1";
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
  services.emacs = {
    enable = true;
    package = myEmacs;
  };

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
      # mu.enable    = true;
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
        # mu.enable    = true;
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
      # mu.enable    = true;
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

  # manually write config files
  # polybar
  xdg.configFile."polybar/config.ini".source = ../../programs/polybar/config.ini;
  xdg.configFile."polybar/launch.sh".source  = ../../programs/polybar/launch.sh;
  # xmonad
  home.file.".xmonad/xmonad.hs".source = ../../programs/xmonad/xmonad_framework.hs;
  # doom emacs
  home.file.".doom.d/".source = ../../programs/emacs/doom;
  home.file.".doom.d/".onChange = "/home/doyougnu/.emacs.d/bin/doom sync";
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
  };

  home.pointerCursor = {
    package = pkgs.numix-cursor-theme;
    name = "Numix-Cursor";
    x11.enable = true;
  };
  ## environment variables
  home.sessionVariables = {
    LIBGL_ALWAYS_SOFTWARE = "1";
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
    prezto.editor.keymap = "vi";
    syntaxHighlighting.enable = true;
    initExtra = ''
      source ~/.p10k.zsh
      alias hg='history | grep'

      function lookup () { sdcv $1 | less }
      function _lookup () { sdcv $1 }

      function lookup-notify () { notify-send -t 2000 "$(lookup $1)" }

      # Remove any custom keybindings for copy/paste in zsh
      bindkey -r '^O'
      bindkey -r '^E'
      bindkey -r '^V'

      bindkey "^p" history-beginning-search-backward
      bindkey "^n" history-beginning-search-forward
      bindkey -v "^p" up-line-or-history
      bindkey -v "^n" down-line-or-history
      bindkey -v "^s" autosuggest-accept

      # Set the autosuggestion highlight style to a lava-like color
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#ff4500'
    '';
    shellAliases = {
      rgf = "rg --files";
      e  = "emacsclient -cn";

      gs = "git status";
      gp = "git push";
      gsu = "git submodule update --recursive";
      nix = "noglob nix";
      nixos-rebuild = "noglob nixos-rebuild";
    };
    sessionVariables = {
      GPG_TTY = "$(tty)";
    };
  };

  home.packages = with pkgs; [
    alsa-utils
    autorandr
    cabal-install
    chez
    cowsay
    cachix
    evince
    entr
    myEmacs
    firefox
    fasd
    feh
    gcc   # for org-roam
    google-chrome
    gauche
    gforth # aoc2023
    # gollum
    haskellPackages.hasktags
    libevent
    libpulseaudio  # for polybar config
    idris2
    libnotify
    killall         # for polybar launch script
    moreutils
    nodejs
    unstable.mu              # for email
    multimarkdown
    pdfpc           # pdf presentaitons from the shell
    polybar         # for xmonad
    pinentry
    pianobar
    python310
    python310Packages.pygments
    ranger
    ripgrep
    rsync
    # rnix-lsp         # for nix lsp in emacs
    sdcv             # for polybar
    signal-desktop
    slack
    spotify
    steam
    tdesktop
    vlc
    xclip
    # xorg.xwininfo    # for emacs everywhere
    # xdotool          # for emacs everywhere
    w3m              # text broser for emacs-w3m
    sqlite
    wordnet
    zip
  ] ++
  [ R-with-packages
  ] ++
  my-fonts
    ++
  (with pkgs;
    [ gmp
      numactl
      valgrind
      flameshot
      discord
      linuxPackages.perf
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
