;;; init.el --- Description -*- lexical-binding: t; -*-
; based on https://arne.me/blog/emacs-from-scratch-part-one-foundations#become-evil
;; thank you for your labor!

;(scroll-bar-mode -2)           ; Hide the always-visible scrollbar
(setq inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)      ; Ask for textual confirmation instead of GUI
(add-to-list 'default-frame-alist '(undecorated . t)) ; remove the window manager frame

;; set up straight.el
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

;; a must have for nix.
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
  (global-prettify-symbols-mode 1)
  (setq compilation-scroll-output t)
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

;; best theme for nw on alacritty
(load-theme 'modus-vivendi t)

(use-package nerd-icons)

(use-package helpful
  :ensure t
  :defer
  :bind (:map global-map
         ("C-h f" . helpful-callable)
         ("C-h F" . helpful-Function)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)
         ("C-h x" . helpful-command)
         ("C-h h" . helpful-at-point)))

(use-package diff-hl
  :config
  (global-diff-hl-mode 1))

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
  (setq backup-directory-alist `((".*" . "~/.emacs.d/saves"))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;; project management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package project-tab-groups
  :ensure
  :demand
  :config
  (project-tab-groups-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; meow ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package move-text
  :ensure t
  :demand t
  :config
  (defun indent-region-advice (&rest ignored)
    "Indent after moving"
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice)

  (global-set-key (kbd "M-t") #'move-text-up)
  (global-set-key (kbd "M-n") #'move-text-down))

(use-package meow
  :ensure t
  :demand t
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (defvar magit-keymap
    (let ((keymap (make-keymap)))
      (define-key keymap (kbd "v") #'magit-status)
      (define-key keymap (kbd "b") #'magit-blame)
      (define-key keymap (kbd "h") #'magit-status-here)
      (define-key keymap (kbd "p") #'diff-hl-show-hunk-previous)
      (define-key keymap (kbd "n") #'diff-hl-show-hunk-next)
      (define-key keymap (kbd "m") #'git-timemachine-toggle)
      (define-key keymap (kbd "t") #'magit-todos-list)
      (define-key keymap (kbd "l") #'magit-log)
      keymap))

  (defvar buffer-keymap
    (let ((keymap (make-keymap)))
      (define-key keymap (kbd "b") #'project-switch-to-buffer)
      (define-key keymap (kbd "l") #'project-list-buffers)
      (define-key keymap (kbd "i") #'ibuffer)
      (define-key keymap (kbd "s") #'save-buffer)
      (define-key keymap (kbd "r") #'revert-buffer)
      (define-key keymap (kbd "d") #'kill-current-buffer)
      (define-key keymap (kbd "x") #'scratch-buffer)
      keymap))

  (defvar notes-keymap
    (let ((keymap (make-keymap)))
      (define-key keymap (kbd "s") #'org-roam-db-sync)
      (define-key keymap (kbd "i") #'org-roam-node-insert)
      (define-key keymap (kbd "f") #'org-roam-node-find)
      (define-key keymap (kbd "r") #'org-roam-buffer-display-dedicated)
      keymap))

  (defvar error-keymap
    (let ((keymap (make-keymap)))
      (define-key keymap (kbd "n") #'next-error)
      (define-key keymap (kbd "p") #'previous-error)
      (define-key keymap (kbd "N") #'flymake-goto-next-error)
      (define-key keymap (kbd "P") #'flymake-goto-prev-error)
      keymap))

  (defvar smerge-keymap
    (let ((keymap (make-keymap)))
      (define-key keymap (kbd "n") #'smerge-next)
      (define-key keymap (kbd "p") #'smerge-prev)
      (define-key keymap (kbd "u") #'magit-smerge-keep-upper)
      (define-key keymap (kbd "l") #'magit-smerge-keep-lower)
      keymap))

  ;; define an alias for your keymap
  (defalias '+magit  magit-keymap)
  (defalias '+buffer buffer-keymap)
  (defalias '+notes  notes-keymap)
  (defalias '+error  error-keymap)
  (defalias '+smerge smerge-keymap)

  (global-set-key (kbd "C-c v") '+magit)
  (global-set-key (kbd "C-c b") '+buffer)
  (global-set-key (kbd "C-c n") '+notes)
  (global-set-key (kbd "C-c e") '+error)
  (global-set-key (kbd "C-c i") '+smerge)

  (defun dyg/meow-save-to-clipboard ()
    "Meow save selection and copy to system clipboard."
    (interactive)
    (meow-save) ; this is the actual saving function
    (when (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (dyg/copy-to-clipboard text)))
    (meow--cancel-selection))

  (defun dyg/copy-to-clipboard (text)
    "Copy TEXT to the system clipboard, handling both Linux and WSL."
    (cond
     ;; WSL clipboard
     ((and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
      (let ((proc (start-process "clip.exe" nil "clip.exe")))
        (process-send-string proc text)
        (process-send-eof proc)))

     ;; Native Linux clipboard via xclip or xsel
     ((eq system-type 'gnu/linux)
      (if (executable-find "xclip")
          (let ((proc (start-process "xclip" nil "xclip" "-selection" "clipboard")))
            (process-send-string proc text)
            (process-send-eof proc))
        (message "xclip not found. Install it for clipboard support.")))

     ;; Windows
     ((eq system-type 'windows-nt)
      (let ((proc (start-process "clip" nil "clip.exe")))
        (process-send-string proc text)
        (process-send-eof proc)))))

  (defun dyg/join-line ()
    "Mimic vims join-line behavior"
    (interactive)
    (forward-line 1)
    (join-line)
    (indent-according-to-mode))

  (defun dyg/paste-from-clipboard ()
    (interactive)
    (insert
     (string-trim-right
      (cond
       ;; WSL
       ((getenv "WSLENV")
        (shell-command-to-string "powershell.exe -command Get-Clipboard"))
       ;; Linux
       ((executable-find "xclip")
        (shell-command-to-string "xclip -selection clipboard -o"))
       ;; Windows
       ((eq system-type 'windows-nt)
        (shell-command-to-string "powershell.exe -command Get-Clipboard"))))))

  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("$" . backward-paragraph)
   '("!" . meow-page-down)
   '("^" . meow-page-up)
   '("_" . forward-paragraph)
   '("T" . avy-goto-char-2)
   '("C" . comment-line)
   '("?" . align-regexp)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; errors
   '("e n" . next-error)
   '("e p" . previous-error)
   '("e N" . flymake-goto-next-error)
   '("e P" . flymake-goto-prev-error)
   ;; misc
   '("s" . highlight-symbol-at-point)
   '("S" . unhighlight-regexp)
   ;; buffers
   '(","   . consult-buffer)
   '("."   . consult-buffer-other-window)
   ;; notes and agenda
   '("a"   . org-agenda)
   '("SPC" . org-capture)
   ;; eval
   '(":"   . eval-expression)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("(" . meow-beginning-of-thing)
   '(")" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . comment-line)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("M" . undo-redo)
   '("n" . meow-search)
   '("N" . dyg/join-line)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . dyg/paste-from-clipboard)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . avy-goto-char-2)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("V" . consult-mark)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . dyg/meow-save-to-clipboard)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   '("$" . backward-paragraph)
   '("!" . meow-page-down)
   '("^" . meow-page-up)
   '("_" . forward-paragraph)
   '(":" . align-regexp)
   '("=" . meow-indent))

  ;; kickoff
  (setq meow-use-clipboard t)
  (meow-global-mode 1)
  (meow-setup-indicator))

(use-package magit
  :ensure t
  :demand
  :config
  (define-key magit-mode-map (kbd "x") #'magit-discard))

(use-package magit-todos
  :ensure t
  :config
  (magit-todos-mode 1))
(use-package git-timemachine :ensure t)

(use-package writeroom-mode
  :ensure t
  :demand
  :config

  (global-set-key (kbd "C-c z") #'writeroom-mode)
  ;; just make the write area a tad larger than wrap
  (setq writeroom-width 90)
  (setq writeroom-fullscreen-effect 'maximized)

  ;; bump font in writeroom
  (add-hook 'writeroom-mode-enable-hook (lambda () (text-scale-set 1)))
  (add-hook 'writeroom-mode-disable-hook (lambda () (text-scale-set 0))))

(use-package writegood-mode
  :ensure t
  :demand
  :config
  (global-set-key (kbd "C-c Z") #'writegood-mode))

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

  ;; (leader-keys
  ;;  "l" '(:ignore t :which-key "lsp")
  ;;  "l <escape>" '(keyboard-escape-quit :which-key t)
  ;;  "l r" '(eglot-rename :which-key "rename")
  ;;  "l a" '(eglot-code-actions :which-key "code actions"))
  )

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package zig-mode
  :ensure t
  :hook (zig-mode . prettify-symbols-mode)
  :hook (zig-mode . display-line-numbers-mode)
  :custom (zig-format-on-save t)
  :config

  (add-hook 'eglot-managed-mode-hook
			(lambda ()
			  (when (derived-mode-p 'zig-mode)
				(eglot-inlay-hints-mode -1))))

  ;; (leader-keys
  ;;   "c" '(:ignore t :which-key "mode")
  ;;   "c <escape>" '(keyboard-escape-quit :which-key t)
  ;;   "c b" '(zig-compile :which-key "build")
  ;;   "c r" '(zig-run :which-key "run")
  ;;   "c t" '(zig-test-buffer :which-key "test")
  ;;   "c f" '(zig-format-buffer :which-key "check")
  ;;   "j"   '(zig-end-of-defun :which-key "end defun")
  ;;   "k"   '(zig-beginning-of-defun :which-key "beg defun"))
  )

(use-package rust-mode
  :init
  ;; TODO (setq rust-mode-treesitter-derive t)
  :hook (rust-mode . prettify-symbols-mode)
  :hook (rust-mode . display-line-numbers-mode)
  :config

  ;; Turn off for inlay hints for rust
  (add-hook 'eglot-managed-mode-hook
			(lambda ()
			  (when (derived-mode-p 'rust-mode)
				(eglot-inlay-hints-mode -1))))
  (setq rust-format-on-save t)
  ;; (leader-keys
  ;;   "c" '(:ignore t :which-key "mode")
  ;;   "c <escape>" '(keyboard-escape-quit :which-key t)
  ;;   "c b" '(rust-compile :which-key "build")
  ;;   "c r" '(rust-run :which-key "run")
  ;;   "c t" '(rust-test :which-key "test")
  ;;   "c k" '(rust-check :which-key "check")
  ;;   "c c" '(rust-run-clippy :which-key "clippy"))
  )

(use-package haskell-mode
  :hook (haskell-mode . haskell-indentation-mode)
  :hook (haskell-mode . prettify-symbols-mode)
  :hook (haskell-mode . display-line-numbers-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  :bind (:map    vertico-map
        ("C-t" . vertico-directory-up)
        ("C-n" . vertico-next)
        ("C-s" . vertico-directory-enter)))

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
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  :init
  (global-corfu-mode)
  :bind (:map corfu-map
              ("C-t" . corfu-previous)
              ("C-n" . corfu-next)
              ("C-s" . corfu-complete)))

(use-package corfu-terminal
  :ensure t
  :demand
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package wgrep
  :defer t
  :config
  (add-hook 'embark-after-export-hook #'wgrep-change-to-wgrep-mode)
  (add-hook 'embark-collect-mode-hook #'wgrep-change-to-wgrep-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c I" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("M-p" . fill-paragraph)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s s" . dyg|consult-ripgrep-word-at-point)
         ("M-s r" . consult-ripgrep)
         ("C-s"   . dyg|consult-line-word-at-point)
         ("M-s i" . consult-imenu-multi)
         ("M-s I" . consult-imenu)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;;;###autoload
  (defun dyg|consult-ripgrep-word-at-point ()
    "Run `consult-ripgrep` preloaded with the word under the cursor."
    (interactive)
    (let ((word (thing-at-point 'word t)))
      (if word
          (consult-ripgrep nil word)
        (consult-ripgrep nil nil))))

  ;;;###autoload
  (defun dyg|consult-line-word-at-point ()
    "Run `consult-line` preloaded with the word under the cursor."
    (interactive)
    (let ((word (thing-at-point 'word t)))
      (if word
          (consult-line nil word)
        (consult-line nil nil))))

  ;; avoid certain buffers in search
  (mapcar
   (lambda (rx) (add-to-list 'consult-buffer-filter rx))
   '("\\`magit\\(?:-[a-z]+\\)?:"
     "\\*Warnings\\*"
     "\\*helpful.*"
     "\\*Help\\*"
     "\\*\\#.*\\#\\*" ;; temp files
     "\\*Async-native-compile-log\\*"
     "\\*Messages\\*"
     "\\`\\*EGLOT .* events\\*\\'"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<"))

(use-package ace-window
  :ensure t
  :demand
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?i ?e ?a ?, ?. ?h ?t ?s ?n)))

(use-package embark
  :ensure t
  :demand
  :bind
  (("C-e" . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;;;###autoload
  (defun ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command. The
     next buffer is the buffer displayed by the next command invoked
     immediately after this command (ignoring reading from the
     minibuffer). Creates a new window before displaying the
     buffer. When `switch-to-buffer-obey-display-actions' is
     non-nil,`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))

  (defvar-keymap my/window-prefix-map
    :doc "Keymap for various window-prefix maps"
    :suppress 'nodigits
    "o" #'ace-window-prefix
    "t" #'same-window-prefix
    "|" #'split-window-horizontally
    "-" #'split-window-vertically
    "w" #'other-window-prefix
    "f" #'other-frame-prefix)

  ;; Look up the key in `my/window-prefix-map' and call that function first.
  ;; Then run the default embark action.
  ;;;###autoload
  (cl-defun my/embark--call-prefix-action (&rest rest &key run type &allow-other-keys)
    (when-let ((cmd (keymap-lookup
                     my/window-prefix-map
                     (key-description (this-command-keys-vector)))))
      (funcall cmd))
    (funcall run :action (embark--default-action type) :type type rest))

  ;; Dummy function, will be overridden by running `embark-around-action-hooks'
  (defun my/embark-set-window () (interactive))

  ;; When running the dummy function, call the prefix action from above
  (setf (alist-get 'my/embark-set-window embark-around-action-hooks)
        '(my/embark--call-prefix-action))

  ;; to support multi-category commands like those provided by Consult
  (setf (alist-get 'buffer embark-default-action-overrides) #'pop-to-buffer-same-window
        (alist-get 'file embark-default-action-overrides) #'find-file
        (alist-get 'bookmark embark-default-action-overrides) #'bookmark-jump
        (alist-get 'library embark-default-action-overrides) #'find-library)

  (map-keymap (lambda (key cmd)
                (keymap-set embark-general-map (key-description (make-vector 1 key))
                            #'my/embark-set-window))
              my/window-prefix-map)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (eval-when-compile
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

  (eval-when-compile
    (defmacro my/embark-split-action (fn split-type)
      `(defun ,(intern (concat "my/embark-"
                               (symbol-name fn)
                               "-"
                               (car (last  (split-string
                                            (symbol-name split-type) "-"))))) ()
         (interactive)
         (funcall #',split-type)
         (call-interactively #',fn))))

  (define-key embark-file-map     (kbd "-") (my/embark-split-action find-file split-window-below))
  (define-key embark-buffer-map   (kbd "-") (my/embark-split-action switch-to-buffer split-window-below))
  (define-key embark-bookmark-map (kbd "-") (my/embark-split-action bookmark-jump split-window-below))

  (define-key embark-file-map     (kbd "|") (my/embark-split-action find-file split-window-right))
  (define-key embark-buffer-map   (kbd "|") (my/embark-split-action switch-to-buffer split-window-right))
  (define-key embark-bookmark-map (kbd "|") (my/embark-split-action bookmark-jump split-window-right))
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; avy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq initial-major-mode 'org-mode)

(use-package emacsql
  :ensure t
  :demand)

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :bind (:map org-mode-map
        ("M-t" . org-insert-heading-respect-content)
        ("M-n" . dyg|org-insert-subheading-respect-content))
  :config

  (defun dyg|org-insert-subheading-respect-content ()
  "Insert a subheading respecting the content below the current heading."
  (interactive)
  (org-insert-heading-respect-content)
  (org-do-demote))

  (defun dyg|org-latex-preview ()
	"Automatically refresh LaTeX fragments in the current buffer."
	(when (eq major-mode 'org-mode)
	  (org-latex-preview)))

  ;; Enable automatic LaTeX fragment preview
  (defun dyg|org-latex-preview-setup ()
  "Set up auto LaTeX fragment preview."
  (add-hook 'after-save-hook #'dyg|org-latex-preview nil t))
  (add-hook 'org-mode-hook #'dyg|org-latex-preview-setup)

  (setq org-startup-with-inline-images t)
  (setq org-M-RET-may-split-line nil)
  (setq org-startup-indented t)
  (setq org-format-latex-options
		(plist-put org-format-latex-options :scale 1.5))
  (add-hook 'org-mode-hook
            #'(lambda ()
                (add-hook 'meow-insert-mode-hook
                          #'(lambda () (setq-local org-hide-emphasis-markers nil)))
                (add-hook 'meow-insert-exit-hook
                          #'(lambda () (setq-local org-hide-emphasis-markers t)))))


  ;; from https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#ensure-blank-lines-between-headings-and-before-contents
  (defun dyg|org-fix-blank-lines ()
    "Ensure that blank lines exist between headings and between
    headings and their contents. Operates on whole buffer."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (org-map-entries (lambda ()
                         (org-with-wide-buffer
                          ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                          ;; newlines before the current heading, so we do this part widened.
                          (while (not (looking-back "\n\n" nil))
                            ;; Insert blank lines before heading.
                            (insert "\n")))
                         (let ((end (org-entry-end-position)))
                           ;; Insert blank lines before entry content
                           (forward-line)
                           (while (and (org-at-planning-p)
                                       (< (point) (point-max)))
                             ;; Skip planning lines
                             (forward-line))
                           (while (re-search-forward org-drawer-regexp end t)
                             ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                             ;; for some reason it doesn't work correctly when operating on hidden text.
                             ;; This works, taken from `org-agenda-get-some-entry-text'.
                             (re-search-forward "^[ \t]*:END:.*\n?" end t)
                             (goto-char (match-end 0)))
                           (unless (or (= (point) (point-max))
                                       (org-at-heading-p)
                                       (looking-at-p "\n"))
                             (insert "\n"))))
                       t nil)))

  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'before-save-hook  #'dyg|org-fix-blank-lines)

  (setq org-directory       "~/sync/org")
  (setq org-clock-idle-time 20)
  (setq org-pretty-entities t)
  (setq org-clock-persist   'history)
  (setq org-journal-file-format                "%Y/%m/%d.org")
  (setq org-columns-default-format             "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-carryover-items           "TODO=\"NEXT\"|TODO=\"TODO\"|TODO=\"HOLD\"|TODO=\"INPROG\"")
  (setq org-confirm-babel-evaluate             nil)
  (setq org-edit-src-content-indentation      2)
  ;; don't set bookmarks on a capture
  (setf org-bookmark-names-plist             nil)
  ;; request a note everytime we clock out on clocked in item
  (setf org-log-note-clock-out           t)
  (org-clock-persistence-insinuate)

  ;; load my personal org agenda and capture config
  (load-user-file "personal-org.el")

  ;; use firefox
  (setf browse-url-browser-function 'browse-url-firefox))

(use-package org-bullets
  :ensure t
  :hook ((org-mode . org-bullets-mode)
         (evil-org-mode . org-bullets-mode)))

(use-package org-roam
  :ensure t
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

  ;;;###autoload
  (defun roam|tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))

  (add-hook 'org-roam-capture-new-node-hook #'roam|tag-new-node-as-draft))

(use-package yasnippet
  :ensure t
  :demand
  :bind* ("M-i" . yas-expand) ;; Global binding for yas-expand
  :config
  (use-package yasnippet-snippets :ensure t)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets/"))
  (yas-global-mode 1)
  ;; (define-key yas-minor-mode-map (kbd "M-o") #'yas-expand)
  (define-key yas-minor-mode-map (kbd "M-i") #'yas-next-field-or-maybe-expand)
  (define-key yas-keymap         (kbd "M-i") #'yas-next-field-or-maybe-expand))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; globals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq global-display-line-numbers-mode t)
(setq whitespace-style '(face spaces trailing tabs space-mark tab-mark))

(use-package emacs
  :init
  (if (not (equal (getenv "EMACS_HOST") "framework"))
      (with-eval-after-load 'whitespace-mode
        (set-face-attribute 'whitespace-space nil
                            :foreground "gray18")
        (set-face-attribute 'whitespace-tab nil
                            :foreground "gray18")))
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (setq global-whitespace-mode 1)
  (which-key-mode)
  (setq which-key-idle-delay 0.10)
  (setq ring-bell-function 'ignore)

  (with-eval-after-load 'eshell-mode
    (define-key eshell-mode-map (kbd "C-n") #'eshell-next-input)
    (define-key eshell-mode-map (kbd "M-n") #'eshell-next-matching-input)
    (define-key eshell-mode-map (kbd "M-p") #'eshell-previous-matching-input))
  )

;; Specifics for Verse
(if (equal (getenv "EMACS_HOST") "thinkpad")
    (progn
      (add-to-list 'load-path "~/.emacs.d/lisp/")
      (require 'verse-mode)))
