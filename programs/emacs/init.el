;; based on https://arne.me/blog/emacs-from-scratch-part-one-foundations#become-evil
;; thank you for your labor!

;; hide the ui elements
(tool-bar-mode -1)             ; Hide the outdated icons
(scroll-bar-mode -1)           ; Hide the always-visible scrollbar
(setq inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)      ; Ask for textual confirmation instead of GUI

;; set up staright.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

;; set up the garbage collection hacks
;; this has to be right after straight to reduce startup times
(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

;; a must have for nix
(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package emacs
  :init
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message "")))

(use-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package emacs
  :init
  (set-charset-priority         'unicode)
  (setq locale-coding-system    'utf-8
        coding-system-for-read  'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system   'utf-8)
  (set-keyboard-coding-system   'utf-8)
  (set-selection-coding-system  'utf-8)
  (prefer-coding-system         'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

(use-package emacs
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2))

(use-package emacs
  :init
  (set-face-attribute 'default nil
                      :font "Source Code Pro"
                      :height 100)
  (set-face-attribute 'fixed-pitch nil
                      :font "Source Code Pro"
                      :height 100)
  (set-face-attribute 'variable-pitch nil
                      :font "Source Code Pro"
                      :height 100))

;; themes
(setq custom-safe-themes t)
(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-challenger-deep t))

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

(use-package nerd-icons)

;; which-key
(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.35) ; Open after .5s instead of 1s
  :config
  (which-key-mode))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emacs
  :init
  ;; allows escape to exit anything
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; hide file icon make titlebar match emacs theme
(use-package emacs
  :init
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil))

(use-package emacs
  :config
  (setq backup-directory-alist `(("." . "~/.saves"))))

(use-package treesit-auto
  :defer
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package emacs
  :init
  (setq-default fill-column 80)
  (set-face-attribute 'fill-column-indicator nil
                      :foreground "#717C7C"
                      :background "transparent")
  (global-display-fill-column-indicator-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use general to setup the evil keybinds
;; TODO use boon, meow, or emacs
(use-package general
  :demand
  :config
  (general-evil-setup)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-.")

  (defun dyg|tab-close ()
    (interactive)
    (eglot-shutdown (eglot-current-server))
    (projectile-kill-buffers)
    (tab-close))

  (leader-keys
    "x" '(scratch-buffer :which-key "*scratch*")

    ;; Buffer
    "b" '(:ignore t :which-key "buffer")

    ;; Buffers
    "b b" '(projectile-switch-to-buffer :which-key "switch buffer")
    "b l" '(buffer-menu                 :which-key "list buffers")
    "b s" '(save-buffer                 :which-key "save buffer")
    "b r" '(revert-buffer               :which-key "revert buffer")
    ;; Don't show an error because SPC b ESC is undefined, just abort
    "b <escape>" '(keyboard-escape-quit :which-key t)
    "b d" 'kill-current-buffer

    ;; workspaces via tabs
    "t"        '(:ignore t :which-key "tabs")
    "t n"      '(tab-new     :which-key "new tab")
    "t t"      '(tab-switch  :which-key "switch tab")
    "t d"      '(dyg|tab-close :which-key "close tab")
    "t h"      '(tab-previous :which-key "previous tab")
    "t l"      '(tab-next     :which-key "next tab")

    ;; notes
    "n"          '(:ignore t :which-key "notes")
    "n r"        '(:ignore t :which-key "roam")
    "n r o"      '(org-roam-node-find     :which-key "find node")
    "n r i"      '(org-roam-node-insert   :which-key "insert node")
    "n r r"      '(org-roam-buffer-toggle :which-key "Toggle roam buffer")

    ;; open
    "o"          '(:ignore t :which-key "open")
    "o o"        '(projectile-find-file   :which-key "find-file")
    "o h"        '(find-file-other-window :which-key "find-file"))

  (general-define-key
   :states '(normal visual)
    "l"   'evil-forward-word-begin
    "h"   'evil-backward-word-end
    "L"   'evil-forward-char
    "H"   'evil-backward-char
    "J"   'evil-forward-paragraph
    "K"   'evil-backward-paragraph
    "C-j" 'evil-join))

(use-package evil
  :demand ; No lazy loading
  :init
  ;; prevents evil and evil-collection from interfering
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t) ;; make C-u scroll up
  :config
  (evil-mode 1))

(use-package evil-lion
  :ensure t
  :bind (:map evil-normal-state-map
         ("g l " . evil-lion-left)
         ("g L " . evil-lion-right)
         :map evil-visual-state-map
         ("g l " . evil-lion-left)
         ("g L " . evil-lion-right))
  :config
  (evil-lion-mode))

(use-package magit
  :config
  (leader-keys
    "g"          '(:ignore t :which-key "magit")
    "g <escape>" '(keyboard-escape-quit :which-key t)
    "g g"        '(magit-status :which-key "status")
    "g l"        '(magit-log :which-key "log"))
  (general-nmap
    "<escape>" #'transient-quit-one))

;; a utility package to get magit to play nice with evil
;; we can already see how much complexity is added due to evil
(use-package evil-collection
  :after evil
  :demand
  :config
  (evil-collection-init))

;; comment lines with gc
(use-package evil-nerd-commenter
  :defer
  :config
  (general-nvmap
    "gc" 'evilnc-comment-operator))

;; projectile, should I try project?
;; projectile must be after general and evil for #'leader-keys
(use-package projectile
  :demand
  :config
  (defun dyg|projectile-switch-project-into-tab ()
    "Open a new tab and then switch to a project"
    (interactive)
    (tab-bar-new-tab -1) ;; create to the left
    (tab-previous)       ;; move to the new tab
    (call-interactively 'projectile-switch-project)
    (tab-bar-rename-tab (projectile-project-name)))

  (leader-keys
   :states 'normal
   "SPC" '(projectile-find-file :which-key "find file")

   ;; Projects
   "p"          '(:ignore t :which-key "projects")
   "p <escape>" '(keyboard-escape-quit            :which-key t)
   "p p"        '(dyg|projectile-switch-project-into-tab :which-key "switch project")
   "p a"        '(projectile-add-known-project    :which-key "add project")
   "p r"        '(projectile-remove-known-project :which-key "remove project")
   "p s"        '(projectile-grep                 :which-key "search project"))
  :init
  (projectile-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eglot, lsp-mode is slow even for rust, bad software
(use-package emacs
  :hook (zig-mode        . eglot-ensure)
  :hook (rust-mode       . eglot-ensure)
  :hook (scheme-mode     . eglot-ensure)
  :hook (haskell-mode    . eglot-ensure)
  :hook (lisp-mode       . eglot-ensure)
  :hook (typescript-mode . eglot-ensure)

  :config
  (leader-keys
   "l" '(:ignore t :which-key "lsp")
   "l <escape>" '(keyboard-escape-quit :which-key t)
   "l r" '(eglot-rename :which-key "rename")
   "l a" '(eglot-code-actions :which-key "code actions")))

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))
(use-package zig-mode
  :config
  (leader-keys
    "m" '(:ignore t :which-key "mode")
    "m <escape>" '(keyboard-escape-quit :which-key t)
    "m b" '(zig-compile :which-key "build")
    "m r" '(zig-run :which-key "run")
    "m t" '(zig-test :which-key "test")))
(use-package rust-mode
  :config
  (leader-keys
    "m" '(:ignore t :which-key "mode")
    "m <escape>" '(keyboard-escape-quit :which-key t)
    "m b" '(rust-compile :which-key "build")
    "m r" '(rust-run :which-key "run")
    "m t" '(rust-test :which-key "test")
    "m k" '(rust-check :which-key "check")
    "m c" '(rust-run-clippy :which-key "clippy")))

(use-package haskell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; avy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :bind (:map evil-normal-state-map
         ("g j " . evil-avy-goto-line-below)
         ("g k " . evil-avy-goto-line-above)
         ("M-t " . evil-avy-goto-char-2)
         :map evil-visual-state-map
         ("g j " . evil-avy-goto-line-below)
         ("g k " . evil-avy-goto-line-above)
         ("M-t " . evil-avy-goto-char-2))
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq initial-major-mode 'org-mode)
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :config

  (setq org-roam-directory (file-truename "~/sync/roam")
        org-roam-inbox     (concat org-roam-directory "/" "refile.org"))

  (setq org-roam-capture-templates
        '(("m" "main" plain "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("l" "literature" plain "%?"
           :if-new
           (file+head "literature/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("c" "concepts" plain "%?"
           :if-new
           (file+head "concepts/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)))

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  (defun roam|tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))

  (add-hook 'org-roam-capture-new-node-hook #'roam|tag-new-node-as-draft))

(use-package yasnippet
  :after evil-org
  :ensure t
  :bind* ("M-o" . yas-expand) ;; Global binding for yas-expand
  :config
  (use-package yasnippet-snippets :ensure t)
  (setq yas-snippet-dirs
        '("/home/doyougnu/.emacs.d/snippets/"))
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "M-o") #'yas-expand))
