{ config, pkgs, lib, ... }:

{

  imports = [];

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
                "*.elc" "*.swp" ".projectile" ".ignored" "*/config.sub" ".envrc"
                "config.sub" "libraries/unix"
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

  services.unison = {
    enable = true;
    # pairs = {
    #   "sync" = {
    #     roots = [ "/home/doyougnu/sync" "ssh://node0@relay.local/home/node0/sync"
    #             ];
    #     commandOptions = { auto = "true";
    #                        batch = "true";
    #                        repeat = "watch";
    #                        copyonconflict = "true";
    #                        ui = "text";
    #                        prefer = "newer";
    #                      };
    #   };
    # };
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
    # theme = "Sea Shells";
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
  home.username = "node0";
  home.homeDirectory = "/home/node0";

  home.activation = {
      # symlinkAuth = lib.hm.dag.entryAfter ["writeBoundary"] ''
      #               ln -sf /home/doyougnu/sync/keys/auth/.authinfo.gpg /home/doyougnu/.authinfo.gpg
      #               '';
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
               {
                 name = "pure";
                 src = pkgs.fetchFromGitHub
                   {
                     owner  = "pure-fish";
                     repo   = "pure";
                     rev    = "1aca7e7a45768af2f5196daa6d37dd2a1d2bb75a";
                     sha256 = "02cf0pd50mj2gh43mlg6s99xfsgrd8zgbqck6mfhlsf1hybvkk04";
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

     function fish_user_key_bindings
       fish_vi_key_bindings
       # bind -M insert \cT history-token-search-backward
       # bind -M insert \cH history-token-search-forward
       # bind -M normal T up-or-search
       # bind -M normal H down-or-search
       bind -M insert tn "if commandline -P; commandline -f cancel; else; set fish_bind_mode default; commandline -f backward-char force-repaint; end"
       bind -M insert \cT up-or-search
       bind -M insert \cH down-or-search
       bind -M visual \cT up-or-search
       bind -M visual \cH down-or-search
       bind -M insert \cS accept-autosuggestion
       bind -M insert -k nul 'accept-autosuggestion execute'
       bind -M visual p  fish_clipboard_paste
     end


     # set pure features
     set --universal pure_show_system_time true
     set --universal pure_show_jobs        true
     set --universal pure_show_prefix_root_prompt         true
     set --universal pure_reverse_prompt_symbol_in_vimode true
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
    guile
    killall         # for polybar launch script
    moreutils
    pinentry
    python310
    python310Packages.pygments
    ripgrep
    rsync
    unison
    zip
  ] ++
  (with unstable;
    [ gmp
      numactl
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
