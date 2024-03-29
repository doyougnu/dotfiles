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
        "ctrl+e" = "copy_to_clipboard";
        "ctrl+u" = "paste_from_clipboard";
      };
    extraConfig = ''
      copy_on_select yes
    '';
    theme = "Chalk";
    settings = {
      font_size = "14.0";
    };
  };

  services.gpg-agent = {
    enable         = true;
    defaultCacheTtl = 34560000;
    maxCacheTtl     = 34560000;
    # pinentryFlavor = "tty";
    extraConfig = ''
    allow-emacs-pinentry
    allow-loopback-pinentry
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

  # systemd.user.services."emacs-daemon" = {
  #   Unit = {
  #     Description = "Start emacs as a daemon";
  #   };
  #   Install = {
  #     WantedBy = [ "default.target" ];
  #   };
  #   Service = {
  #     ExecStart = "/home/doyougnu/.emacs.d/bin/doom sync; ${myEmacs} --fg-daemon";
  #     RemainAfterExit = "yes";
  #     Type = "oneshot";
  #   };
  # };

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

  ## environment variables
  home.sessionVariables = {
    LIBGL_ALWAYS_SOFTWARE = "1";
  };

  programs.fish = {
    enable = true;
    shellAliases = {
      hg = "history | grep";
      e  = "emacsclient -cn";

      gs = "git status";
      gp = "git push";
      gsu = "git submodule update --recursive";

      rgf = "rg --files";

      nsr = "nix-shell --pure --run";
      nr  = "nix-shell  --run";
      nsc = "nix-shell --pure --command";
      ns  = "nix-shell";
      nsp = "nix-shell -p";

      hbR   = "hadrian/build clean && ./boot && ./configure && hadrian/build -j12 --flavour=perf";
      hbc   = "hadrian/build clean && hadrian/build -j12 --flavour=perf";
      hb    = "hadrian/build -j12 --flavour=perf";
      hbq   = "hb --flavour=quick";
      hbqs  = "hbq --skip='//*.mk' --skip='stage1:lib:rts'";
      hbqf  = "hbqs --freeze1";
      hbv   = "hb --flavour=validate --build-root=_validate";
      hbvs  = "hbv --skip='//*.mk' --skip='stage1:lib:rts'";
      hbvf  = "hbvs --freeze1";
      hbt   = "mkdir -p _ticky; [ -e _ticky/hadrian.settings ] || echo -e \"stage1.*.ghc.hs.opts += -ticky -ddump-simpl -ddump-stg-final -ddump-to-file \\nstage1.ghc-bin.ghc.link.o pts += -ticky -ddump-simpl -ddump-stg-final -ddump-to-file\" > _ticky/hadrian.settings; hadrian/build -j12 --flavour=perf --build-root=_ticky";
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
               { # great prompt
                 name = "hydro";
                 src  = pkgs.fishPlugins.hydro.src;
               }
               { # colorize command output
                 name = "puffer-fish";
                 src  = pkgs.fishPlugins.puffer.src;
               }
               { # don't log failed commands
                 name = "sponge";
                 src  = pkgs.fishPlugins.sponge.src;
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
     function fish_user_key_bindings
       fish_vi_key_bindings
       # bind -M insert \cT history-token-search-backward
       # bind -M insert \cN history-token-search-forward
       # bind -M normal T up-or-search
       # bind -M normal N down-or-search
       bind -M insert tn "if commandline -P; commandline -f cancel; else; set fish_bind_mode default; commandline -f backward-char force-repaint; end"
       bind -M insert \cT up-or-search
       bind -M insert \cN down-or-search
       bind -M visual \cT up-or-search
       bind -M visual \cN down-or-search
       bind -M insert \cS accept-autosuggestion
       bind -M insert -k nul 'accept-autosuggestion execute'
       bind -M visual p  fish_clipboard_paste
     end

     # set pure features
     # set --universal pure_show_system_time true
     # set --universal pure_show_jobs        true
     # set --universal pure_show_prefix_root_prompt         true
     # set --universal pure_reverse_prompt_symbol_in_vimode true

     # set hydro features
     set --universal hydro_color_pwd $fish_color_cwd
     set --universal hydro_color_git $fish_color_comment


     fish_config theme choose Lava
     '';

  };

  ## manually load the plugins
  xdg.configFile."fish/conf.d/plugin-pure.fish".text = pkgs.lib.mkAfter ''
  for f in $plugin_dir/*.fish
    source $f
  end
  '';

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
