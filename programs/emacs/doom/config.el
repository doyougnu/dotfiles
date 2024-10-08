;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jeff Young"
      user-mail-address "jeff@doyougnu.xyz")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; I set the font according to monitor size in dyg/config.el
(setq font-family "Source Code Pro")

(after! doom
  (if (equal (getenv "EMACS_HOST") "framework")
    (setq doom-font (font-spec :family font-family :size 18)
          doom-big-font (font-spec :family font-family :size 24))
    (setq doom-font (font-spec :family font-family :size 11)
          doom-big-font (font-spec :family font-family :size 13))))

;; set private config to dotfiles not nix-store copy
(setq doom-user-dir "~/dotfiles/programs/emacs/doom")

(setq evil-escape-key-sequence "hh")
(setq evil-escape-delay 0.2)
(setq evil-snipe-override-evil-repeat-keys 1)
(setq doom-localleader-key     ",")
(setq doom-localleader-alt-key "C-.")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-ir-black)
;; (setq doom-theme 'doom-solarized-dark-high-contrast)
;; (setq doom-theme 'doom-gruvbox-light)
;; (setq doom-theme 'doom-outrun-electric)

(if (equal (getenv "EMACS_HOST") "framework")
    (setq doom-theme 'doom-gruvbox-light)
    (setq doom-theme 'doom-rouge))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq tab-always-indent t)

;; set GC limit to 512 Mebibytes
(setq gc-cons-threshold 536870912)
;; garbage collect when not in focus
(add-function :after
                  after-focus-change-function
                  (lambda () (unless (frame-focus-state) (garbage-collect))))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
