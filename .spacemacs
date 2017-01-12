;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     javascript
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     clojure
     auto-completion
     better-defaults
     emacs-lisp
     git
     elixir
     spotify
     erc
     org
     spell-checking
     markdown
     haskell
     idris
     evernote
     python
     c-c++
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     syntax-checking
     version-control
     ;;myLayers
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(company-coq)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '( zenburn
                         solarized-dark
                         solarized-light
                         spacemacs-light
                         spacemacs-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode t
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   -default-package-repository nil
   tspacemacs/config ()
   )
  ;; User initialization goes here
  (defun dotspacemacs/config ()
      "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
      ;; Make evil-mode up/down operate in screen lines instead of logical lines
      (define-key evil-motion-state-map "j" 'evil-next-visual-line)
      (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
      ;; Also in visual mode
      (define-key evil-visual-state-map "j" 'evil-next-visual-line)
      (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

      (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
      (add-to-list 'exec-path "~/.local/bin/")

      ;; enable hindent
      (setq-default dotspacemacs-configuration-layers
      '((haskell :variables haskell-enable-hindent-style "chris-done")))

      ;; force REPL to stack REPL
      (setq-default dotspacemacs-configuration-layers
      '((haskell :variables haskell-process-type 'stack-ghci)))

      ;; set haskell autocompletion to intero
      (setq-default dotspacemacs-configuration-layers
                    '(auto-completion
                      (haskell :variables haskell-completion-backend 'intero)))

      ;; clojure mode config
      ;;(require 'clojure-mode-extra-font-locking ;;This line messes up terminal colors for some reason)
      (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
      ;;(add-hook 'clojure-mode-hook #'evil-smartparens-mode)
      (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
      (setq clojure-enable-fancify-symbols t)
      ;; toggle comments
      (define-key evil-normal-state-map ",c " " cl")
      (global-linum-mode)
      ;; match paredit.vim key-binding
      (define-key evil-normal-state-map ",W" " kw")  ; wrap with ()
      (define-key evil-normal-state-map ",w["        ; wrap with []
        (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")))
      (define-key evil-normal-state-map ",w{"        ; wrap with {}
        (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{")))

      ;; Spell Checker Configuration
      ;; Disable by default
      (setq-default dotspacemacs-configuration-layers
                    '((spell-checking :variables spell-checking-enable-by-default nil)))

      ;; Enable auto-dictionary mode
      (setq-default dotspacemacs-configuration-layers
                    '((spell-checking :variables spell-checking-enable-auto-dictionary t)))

      ;; Enable auto-completion on mispelled word
      (setq-default dotspacemacs-configuration-layers
                    '((spell-checking :variables =enable-flyspell-auto-completion= t)))
      )
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-projectile pcache org-present org-pomodoro alert log4e gntp org-download htmlize gnuplot web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern dash-functional tern coffee-mode zeal-at-point yapfify ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spotify spaceline powerline smeargle restart-emacs rainbow-delimiters pyvenv pytest pyenv-mode py-isort popwin pip-requirements persp-mode pcre2el paradox orgit org-plus-contrib org-bullets open-junk-file ob-elixir org neotree mwim move-text mmm-mode markdown-toc markdown-mode magit-gitflow macrostep lorem-ipsum live-py-mode linum-relative link-hint intero info+ indent-guide ido-vertical-mode hy-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-spotify multi helm-pydoc helm-projectile helm-mode-manager helm-make projectile helm-hoogle helm-gitignore request helm-flx helm-descbinds helm-dash helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md geeknote flycheck-pos-tip pos-tip flycheck-mix flycheck-haskell flycheck flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks elisp-slime-nav dumb-jump disaster diminish diff-hl define-word cython-mode company-statistics company-ghci company-ghc ghc haskell-mode company-coq company-math math-symbol-lists company-cabal company-c-headers company-anaconda column-enforce-mode cmm-mode cmake-mode clojure-snippets clj-refactor hydra inflections edn multiple-cursors paredit peg clean-aindent-mode clang-format cider-eval-sexp-fu eval-sexp-fu highlight cider seq spinner queue clojure-mode bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed anaconda-mode pythonic f s alchemist company dash elixir-mode pkg-info epl aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell auto-complete zenburn-theme spacemacs-theme quelpa helm avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
