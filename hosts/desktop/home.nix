{ config, pkgs, lib, ... }:

let
    myEmacs = import ../../programs/emacs/emacs.nix { pkgs = pkgs; config = config; unstable = pkgs; };
    haskell-env = with pkgs.haskell.packages.${config.ghc.version}; [
    ];

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
                "*.elc" "*.swp" ".projectile" ".ignored" "*/config.sub"
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
    defaultCacheTtl = 34560000;
    maxCacheTtl     = 34560000;
    # pinentryFlavor = "tty";
    extraConfig = ''
    allow-emacs-pinentry
    allow-loopback-pinentry
    '';
  };

  # kitty config
  programs.kitty = {
    enable = true;
      keybindings = {
        "ctrl+d" = "copy_to_clipboard";
        "ctrl+f" = "paste_from_clipboard";
      };
    extraConfig = ''
      copy_on_select yes
    '';
    theme = "Sea Shells";
    settings = {
      font_size = "9.0";
    };
  };

  services.lorri.enable = true;
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
  services.emacs.enable = true;
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
  xdg.configFile."polybar/config.ini".source    = ../../programs/polybar/config_desktop;
  xdg.configFile."polybar/launch.sh".source = ../../programs/polybar/launch.sh;
  # xmonad
  home.file.".xmonad/xmonad.hs".source = ../../programs/xmonad/xmonad_desktop.hs;
  # doom emacs
  home.file.".doom.d/".source = ../../programs/emacs/doom;
  # this would be nice but it keeps erroring out
  home.file.".doom.d/".onChange= "/home/doyougnu/.emacs.d/bin/doom sync";
  # symlink auth on new hm generation activation
  home.activation = {
      symlinkAuth = lib.hm.dag.entryAfter ["writeBoundary"] ''
                    ln -sf /home/doyougnu/sync/keys/auth/.authinfo.gpg /home/doyougnu/.authinfo.gpg
                    '';
  };

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

      hbR   = "nix-shell --pure --run 'hadrian/build clean && ./boot && ./configure && hadrian/build -j12 --flavour=perf'";
      hbc   = "nix-shell --pure --run 'hadrian/build clean && hadrian/build -j12 --flavour=perf'";
      hb    = "nix-shell --pure --run 'hadrian/build -j12 --flavour=perf'";
      hbq   = "hb --flavour=quick";
      hbqs  = "hbq --skip='//*.mk' --skip='stage1:lib:rts'";
      hbqf  = "hbqs --freeze1";
      hbv   = "hb --flavour=validate --build-root=_validate";
      hbvs  = "hbv --skip='//*.mk' --skip='stage1:lib:rts'";
      hbvf  = "hbvs --freeze1";
      hbt   = "nix-shell --pure --run 'mkdir -p _ticky; [ -e _ticky/hadrian.settings ] || echo -e \"stage1.*.ghc.hs.opts += -ticky -ddump-simpl -ddump-stg-final -ddump-to-file \\nstage1.ghc-bin.ghc.link.o pts += -ticky -ddump-simpl -ddump-stg-final -ddump-to-file\" > _ticky/hadrian.settings; hadrian/build -j12 --flavour=perf --build-root=_ticky'";
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
       # bind -M insert \cT history-token-search-backward
       # bind -M insert \cH history-token-search-forward
       # bind -M normal T up-or-search
       # bind -M normal H down-or-search
       # bind -M insert fd "if commandline -P; commandline -f cancel; else; set fish_bind_mode default; commandline -f backward-char force-repaint; end"
       bind -M insert \cT up-or-search
       bind -M insert \cH down-or-search
       bind -M visual \cT up-or-search
       bind -M visual \cH down-or-search
       bind -M insert \cN accept-autosuggestion
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

  home.packages = with pkgs; [
    alsa-utils
    cbqn
    chez
    cowsay
    discord
    entr
    evince
    fasd
    gerbil
    google-chrome
    guile
    idris2
    libevent
    killall         # for polybar launch script
    ktouch          # learn dvorak!
    moreutils
    myEmacs
    multimarkdown
    nodejs-18_x
    pdfpc           # pdf presentaitons from the shell
    polybar
    pinentry
    pianobar
    python310
    python310Packages.pygments
    qutebrowser
    ranger
    ripgrep
    rnix-lsp
    rsync
    lispPackages.quicklisp
    # sbcl
    sdcv             # for polybar
    signal-desktop
    spotify
    spotify-unwrapped
    slack
    steam
    texlive.combined.scheme-full
    xclip
    xorg.xwininfo    # for emacs everywhere
    xdotool          # for emacs everywhere
    xdg-dbus-proxy
    xdg-desktop-portal
    w3m              # text browser for emacs-w3m
    sqlite
    wordnet
    zip
  ] ++
  [ R
    haskellPackages.hasktags
    haskellPackages.hlint
  ]
    ++
    haskell-env
    ++
  (with unstable;
    [ gmp
      numactl
      flameshot
      tdesktop
      valgrind
      thunderbird
      firefox
      mu     # for email
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
